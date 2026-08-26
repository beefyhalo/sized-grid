{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Candidate build paths for a stencil table, for @sized-grid-kb38@.
--
-- Everything here computes the same pair the library's
-- 'Data.Grid.Sized.Stencil.stencilFor' computes --- a width and a row-major
-- @'VU.Vector' Int@ of that stride --- by a different route. The pair rather
-- than a 'Data.Grid.Sized.Stencil.Stencil' because that type is abstract and
-- has no constructor; its two accessors are what the benchmark compares
-- against.
--
-- The axes of the experiment are two, and they are separable:
--
-- * /how many passes/. The library asks the neighbourhood function twice per
--   cell: once for a length, to discover the width, and once for the row.
--   That is deliberate (@sized-grid-adr.15@): the one-pass alternative it
--   replaced held every row live across the width pass. But a caller who can
--   /bound/ the width --- @mooreStencil@ can, @(2r+1)^d - 1@ --- can lay the
--   table out at the bound in one pass, record the width it actually saw, and
--   compact. Nothing is retained and the neighbourhood function runs once.
-- * /how many cores/. Every cell's row is independent of every other's, so
--   both the width pass and the fill split by index range with no
--   communication at all. The write target is one mutable vector whose
--   chunks are disjoint.
--
-- The parallel variants are pure functions with 'unsafePerformIO' inside.
-- That is not a shortcut around anything: the workers write disjoint slices
-- of a vector that nothing else can see, join before it is frozen, and
-- propagate the first exception any of them raised, so the result is a
-- function of the arguments alone. It is, however, a design decision the
-- issue has to make deliberately, which is why it is measured here first.
module ParStencil
  ( Table(..)
    -- * Sequential
  , tableSeq
  , tableSeqBounded
    -- * Parallel
  , tableParWidth
  , tableParFill
  , tableParBoth
  , tableParBounded
  , tableParChunked
    -- * Shared plumbing
  , chunkRanges
  , mooreBound
  ) where

import           Data.Grid.Sized.Coord

import           Control.Concurrent          (forkOn, getNumCapabilities)
import           Control.Concurrent.MVar     (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception           (SomeException, evaluate, throwIO,
                                              try)
import           Control.Monad               (forM, forM_)
import           Data.Kind                   (Type)
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           System.IO.Unsafe            (unsafePerformIO)

-- | A built table: the stride, and the positions at that stride.
--
-- Strict in both, so forcing one of these to WHNF is forcing the whole table
-- --- an unboxed vector has no interior thunks --- which is what makes
-- @whnf@ the right harness call for a build benchmark.
data Table = Table
    { tblWidth     :: !Int
    , tblPositions :: !(VU.Vector Int)
    } deriving (Eq, Show)

-- | The row of cell @i@, as positions. The one thing every variant below
-- computes, and the only place the neighbourhood function is consulted.
--
-- 'unsafeCoordFromPosition' rather than 'coordFromPosition': @i@ comes from a
-- range this module built out of 'coordSpaceSize', so the check would be
-- discharging a fact already in hand.
rowOf :: forall cs. (Coord cs -> [Coord cs]) -> Int -> [Int]
rowOf f i = map coordPosition (f (unsafeCoordFromPosition i))
{-# INLINE rowOf #-}

-- | @chunkRanges k n@ splits @[0, n)@ into at most @k@ half-open ranges of
-- as near the same length as they divide.
--
-- Static split rather than a work queue. The rows of a bounded grid are not
-- all the same cost --- an edge cell has fewer neighbours than an interior
-- one --- but the imbalance is a boundary effect, so it shrinks as the grid
-- grows, and a queue costs an atomic per row on every grid.
chunkRanges :: Int -> Int -> [(Int, Int)]
chunkRanges k n
    | k <= 1 || n <= 0 = [(0, n) | n > 0]
    | otherwise = go 0 0
  where
    q = n `quot` k
    r = n `rem` k
    go !c !lo
        | c >= k || lo >= n = []
        | otherwise =
            let len = q + (if c < r then 1 else 0)
                hi = lo + len
             in (lo, hi) : go (c + 1) hi

-- | The largest number of neighbours a Moore neighbourhood of radius @r@ can
-- name on @d@ axes: the cube around the cell, less the cell.
--
-- An over-estimate in general --- a bounded axis drops the neighbours that
-- fall off it, and an axis shorter than @2r+1@ reaches the same cell twice
-- and gets deduplicated --- which is exactly why the library discovers the
-- width instead of deriving it. Used here only as an /allocation/ bound: the
-- width that ends up in the 'Table' is still the one the fill observed.
mooreBound :: Int -> Int -> Int
mooreBound r d = (2 * r + 1) ^ d - 1

-- | Run the given actions on separate capabilities and wait for all of them.
--
-- 'forkOn' rather than 'forkIO' because the chunks are equal-sized and
-- long-lived: there is nothing for the scheduler's load balancing to
-- discover, and pinning keeps a chunk's writes on the core whose cache
-- already holds them.
--
-- An exception in a worker is re-raised in the caller rather than printed to
-- stderr and dropped, which is what a bare 'forkOn' would do with it. Without
-- this, a partial neighbourhood function would turn a crash into a silently
-- wrong table.
inParallel :: [IO a] -> IO [a]
inParallel acts = do
    caps <- getNumCapabilities
    slots <-
        forM (zip [0 ..] acts) $ \(i, act) -> do
            slot <- newEmptyMVar
            _ <- forkOn (i `mod` caps) (try (act >>= evaluate) >>= putMVar slot)
            pure slot
    results <- mapM takeMVar slots
    forM results $ either (throwIO @SomeException) pure

-- * Sequential

-- | The library's own build path, transliterated.
--
-- Not the control --- the control is the library's 'stencilFor' itself,
-- reached through its @.hi@ file the way a consumer reaches it. This is here
-- so that the parallel variants differ from something in this module by
-- exactly the parallelism, and the benchmark can show that this and the real
-- one measure the same.
tableSeq :: forall (cs :: [Type]). IsCoordList cs => (Coord cs -> [Coord cs]) -> Table
tableSeq f = Table w (VU.fromListN (n * w) (concatMap padRow [0 .. n - 1]))
  where
    n = coordSpaceSize @cs
    w = maximum (0 : [length (f (unsafeCoordFromPosition i)) | i <- [0 .. n - 1]])
    padRow i = go w (rowOf f i)
      where
        go 0 _      = []
        go k []     = (-1) : go (k - 1) []
        go k (p:ps) = p : go (k - 1) ps

-- | One pass, laid out at a caller-supplied upper bound on the width and
-- compacted to the width actually seen.
--
-- The neighbourhood function runs once per cell rather than twice, and no row
-- outlives the write that consumes it, so this is not the one-pass variant
-- @sized-grid-adr.15@ rejected --- that one kept every row live to learn the
-- maximum. What it costs instead is a table allocated at @ub@ and, when the
-- bound is not tight, a compacting copy.
--
-- __Precondition:__ every row @f@ produces is at most @ub@ long. Unchecked
-- here; a longer row would write over its neighbour.
tableSeqBounded ::
       forall (cs :: [Type]). IsCoordList cs
    => Int
    -> (Coord cs -> [Coord cs])
    -> Table
tableSeqBounded ub f =
    unsafePerformIO $ do
        buf <- VUM.new (n * ub)
        w <- fillRange buf ub (rowOf f) 0 n
        compactTo buf ub w n
  where
    n = coordSpaceSize @cs

-- * Parallel

-- | Parallel width pass, sequential fill. One half of 'tableParBoth', to say
-- which half the speedup came from.
tableParWidth ::
       forall (cs :: [Type]). IsCoordList cs
    => Int
    -> (Coord cs -> [Coord cs])
    -> Table
tableParWidth mult f =
    unsafePerformIO $ do
        chunks <- ranges mult n
        w <- widthPar chunks (rowLen f)
        buf <- VUM.new (n * w)
        _ <- fillRange buf w (rowOf f) 0 n
        Table w <$> VU.unsafeFreeze buf
  where
    n = coordSpaceSize @cs

-- | Sequential width pass, parallel fill. The other half.
tableParFill ::
       forall (cs :: [Type]). IsCoordList cs
    => Int
    -> (Coord cs -> [Coord cs])
    -> Table
tableParFill mult f =
    unsafePerformIO $ do
        chunks <- ranges mult n
        let w = maximum (0 : map (rowLen f) [0 .. n - 1])
        buf <- VUM.new (n * w)
        _ <- inParallel [fillRange buf w (rowOf f) lo hi | (lo, hi) <- chunks]
        Table w <$> VU.unsafeFreeze buf
  where
    n = coordSpaceSize @cs

-- | Both passes parallel: the direct parallelisation of the library's build,
-- same two passes, same absence of retention, split by index range.
tableParBoth ::
       forall (cs :: [Type]). IsCoordList cs
    => Int
    -> (Coord cs -> [Coord cs])
    -> Table
tableParBoth mult f =
    unsafePerformIO $ do
        chunks <- ranges mult n
        w <- widthPar chunks (rowLen f)
        buf <- VUM.new (n * w)
        _ <- inParallel [fillRange buf w (rowOf f) lo hi | (lo, hi) <- chunks]
        Table w <$> VU.unsafeFreeze buf
  where
    n = coordSpaceSize @cs

-- | 'tableSeqBounded' with the fill and the compaction split by index range.
-- One pass over the neighbourhood function, on every core.
tableParBounded ::
       forall (cs :: [Type]). IsCoordList cs
    => Int
    -> Int
    -> (Coord cs -> [Coord cs])
    -> Table
tableParBounded mult ub f =
    unsafePerformIO $ do
        chunks <- ranges mult n
        buf <- VUM.new (n * ub)
        ws <- inParallel [fillRange buf ub (rowOf f) lo hi | (lo, hi) <- chunks]
        let w = maximum (0 : ws)
        if w == ub
            then Table w <$> VU.unsafeFreeze buf
            else do
                out <- VUM.new (n * w)
                _ <- inParallel [copyRows buf ub out w lo hi | (lo, hi) <- chunks]
                Table w <$> VU.unsafeFreeze out
  where
    n = coordSpaceSize @cs

-- | One pass with no bound needed: each chunk lays its rows out at its own
-- observed width, and a second parallel pass copies the chunks into a table
-- of the widest of them.
--
-- The general-purpose one-pass answer, for a neighbourhood function nothing
-- knows a bound for. What it costs is the chunk tables: they are all live at
-- the moment the final one is allocated, so peak memory is two copies of the
-- table rather than one.
tableParChunked ::
       forall (cs :: [Type]). IsCoordList cs
    => Int
    -> (Coord cs -> [Coord cs])
    -> Table
tableParChunked mult f =
    unsafePerformIO $ do
        chunks <- ranges mult n
        parts <- inParallel [chunkTable f lo hi | (lo, hi) <- chunks]
        let w = maximum (0 : [cw | (cw, _) <- parts])
        out <- VUM.new (n * w)
        _ <-
            inParallel
                [ copyFrozenRows part cw out w lo hi
                | ((lo, hi), (cw, part)) <- zip chunks parts
                ]
        Table w <$> VU.unsafeFreeze out
  where
    n = coordSpaceSize @cs

-- * Plumbing

-- | The chunk ranges for a grid of @n@ cells at @mult@ chunks per capability.
ranges :: Int -> Int -> IO [(Int, Int)]
ranges mult n = do
    caps <- getNumCapabilities
    pure (chunkRanges (max 1 (caps * mult)) n)

rowLen :: (Coord cs -> [Coord cs]) -> Int -> Int
rowLen f i = length (f (unsafeCoordFromPosition i))
{-# INLINE rowLen #-}

-- | The largest row length over every chunk, computed a chunk at a time.
widthPar :: [(Int, Int)] -> (Int -> Int) -> IO Int
widthPar chunks len = do
    ws <- inParallel [evaluate (go lo hi 0) | (lo, hi) <- chunks]
    pure (maximum (0 : ws))
  where
    go !i !hi !acc
        | i >= hi = acc
        | otherwise = go (i + 1) hi (max acc (len i))

-- | Write rows @[lo, hi)@ into @buf@ at stride @w@, padding each with the
-- sentinel, and return the longest row seen.
--
-- The returned maximum is what makes the bounded variants one-pass: the width
-- falls out of the fill rather than needing a pass of its own.
fillRange :: VUM.IOVector Int -> Int -> (Int -> [Int]) -> Int -> Int -> IO Int
fillRange buf w row lo hi = go lo 0
  where
    go !i !seen
        | i >= hi = pure seen
        | otherwise = do
            let base = i * w
            end <- write base (row i)
            pad end (base + w)
            go (i + 1) (max seen (end - base))
    write !j [] = pure j
    write !j (p:ps) = VUM.unsafeWrite buf j p >> write (j + 1) ps
    pad !j !end
        | j >= end = pure ()
        | otherwise = VUM.unsafeWrite buf j (-1) >> pad (j + 1) end

-- | Narrow a table laid out at stride @from@ to stride @to@, rows @[lo, hi)@.
copyRows :: VUM.IOVector Int -> Int -> VUM.IOVector Int -> Int -> Int -> Int -> IO ()
copyRows src from dst to lo hi =
    forM_ [lo .. hi - 1] $ \i ->
        VUM.unsafeCopy
            (VUM.unsafeSlice (i * to) to dst)
            (VUM.unsafeSlice (i * from) to src)

-- | 'copyRows' out of a frozen chunk table, whose rows are numbered from
-- @lo@ in the destination and from zero in the source, and which may be
-- narrower than the destination and so need repadding.
copyFrozenRows :: VU.Vector Int -> Int -> VUM.IOVector Int -> Int -> Int -> Int -> IO ()
copyFrozenRows src from dst to lo hi =
    forM_ [lo .. hi - 1] $ \i -> do
        let srcRow = (i - lo) * from
            dstRow = i * to
        VU.unsafeCopy
            (VUM.unsafeSlice dstRow from dst)
            (VU.unsafeSlice srcRow from src)
        forM_ [dstRow + from .. dstRow + to - 1] $ \j ->
            VUM.unsafeWrite dst j (-1)

-- | Rows @[lo, hi)@ laid out at the widest row among them.
chunkTable :: (Coord cs -> [Coord cs]) -> Int -> Int -> IO (Int, VU.Vector Int)
chunkTable f lo hi = do
    let rows = [rowOf f i | i <- [lo .. hi - 1]]
        cw = maximum (0 : map length rows)
        len = (hi - lo) * cw
    pure (cw, VU.fromListN len (concatMap (padTo cw) rows))
  where
    padTo w r = take w (r ++ repeat (-1))

-- | Freeze a buffer laid out at @ub@ as a table of width @w@, copying only if
-- the bound was loose.
compactTo :: VUM.IOVector Int -> Int -> Int -> Int -> IO Table
compactTo buf ub w n
    | w == ub = Table w <$> VU.unsafeFreeze buf
    | otherwise = do
        out <- VUM.new (n * w)
        copyRows buf ub out w 0 n
        Table w <$> VU.unsafeFreeze out
