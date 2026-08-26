-- | Candidate execution paths for a stencil, for @sized-grid-49hi@.
--
-- Everything here computes what the library's
-- 'Data.Grid.Sized.Stencil.stencilGrid' and
-- 'Data.Grid.Sized.Stencil.stencilFoldGrid' compute --- a new grid of the
-- same shape --- by a different route. The stencil itself is an input:
-- building one is @sized-grid-kb38@ and is answered; this is only the
-- running of it.
--
-- The axes of the experiment are two, and as in kb38 it matters a great deal
-- that they are separable:
--
-- * /how strict/. The library fills the result with
--   'Data.Vector.Generic.generate', which on a boxed vector writes a thunk
--   per cell. On an unboxed grid that is the same as forcing; on a boxed one
--   it defers every rule application to whoever forces the grid, and an
--   @iterate@ loop defers them across generations. Writing @$!@ into a
--   mutable vector instead forces each cell where it is computed. That is a
--   change of strictness with no threads in it at all, and it has to be
--   measured on its own or it will be credited to the threads.
-- * /how many cores/. Cell @i@ of the result reads the old grid and the
--   table and writes nothing but slot @i@, so the fill splits by index range
--   with no communication: disjoint slices of one mutable vector, joined
--   before it is frozen.
--
-- Crossing the two gives the four variants each shape is measured at:
-- @Seq@ (the library), @SeqStrict@ (strict, one core), @Par@ (strict, many
-- cores) and @ParSpark@ (many cores without 'unsafePerformIO').
--
-- The @Par@ variants are pure functions with 'unsafePerformIO' inside, on
-- the same licence kb38's build variants took: the workers write disjoint
-- slices of a vector nothing else can see, join before it is frozen, and
-- re-raise the first exception any of them raised, so the result is a
-- function of the arguments alone. @ParSpark@ is the same split expressed
-- with 'par' over per-chunk vectors, which needs no such licence and pays a
-- copy for it; it is here so the API decision can see the price of purity.
module ParExec
  ( -- * Gather-shaped, against 'Data.Grid.Sized.Stencil.stencilGrid'
    gridSeq
  , gridSeqInline
  , gridSeqStrict
  , gridPar
  , gridParSpark
    -- * Fold-shaped, against 'Data.Grid.Sized.Stencil.stencilFoldGrid'
  , foldSeq
  , foldSeqInline
  , foldSeqStrict
  , foldPar
  , foldParSpark
    -- * Shared plumbing
  , chunkRanges
  ) where

import           Data.Grid.Sized              (GridOf, gridVector)
import           Data.Grid.Sized.Stencil      (Stencil, stencilPositions,
                                               stencilWidth)
import           Data.Grid.Sized.Unsafe       (unsafeGridFromVector)

import           Control.Concurrent           (forkOn, getNumCapabilities)
import           Control.Concurrent.MVar      (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception            (SomeException, evaluate, throwIO,
                                               try)
import           Control.Monad                (forM)
import           Control.Monad.ST             (runST)
import           GHC.Conc                     (numCapabilities, par, pseq)
import qualified Data.Vector.Generic          as VG
import qualified Data.Vector.Generic.Mutable  as VGM
import qualified Data.Vector.Unboxed          as VU
import           System.IO.Unsafe             (unsafePerformIO)

-- * Reading a row
--
-- Copied from "Data.Grid.Sized.Stencil" rather than imported: both are
-- private to it, and a spike that reimplemented them differently would be
-- measuring a different program. They are byte-for-byte the library's, so
-- the only thing that differs between a variant here and the control is the
-- fill around them.

-- | The half-open span of the table that holds row @i@ at width @w@.
rowSpan :: Int -> Int -> (Int, Int)
rowSpan w i = (i * w, (i + 1) * w)
{-# INLINE rowSpan #-}

-- | Row @i@ of the table, as the elements it names. Lazy in the tail, ending
-- at the first sentinel.
gatherRow :: VG.Vector v a => VU.Vector Int -> Int -> v a -> Int -> [a]
gatherRow tbl w v i = go start
  where
    (start, end) = rowSpan w i
    go !j
        | j >= end = []
        | otherwise =
            case VU.unsafeIndex tbl j of
                -1 -> []
                p  -> VG.unsafeIndex v p : go (j + 1)
{-# INLINE gatherRow #-}

-- | Row @i@ of the table, folded left instead of gathered into a list.
foldRow' ::
       VG.Vector v a
    => VU.Vector Int
    -> Int
    -> v a
    -> Int
    -> (b -> a -> b)
    -> b
    -> b
foldRow' tbl w v i step = go start
  where
    (start, end) = rowSpan w i
    go !j !acc
        | j >= end = acc
        | otherwise =
            case VU.unsafeIndex tbl j of
                -1 -> acc
                p  -> go (j + 1) (step acc (VG.unsafeIndex v p))
{-# INLINE foldRow' #-}

-- * Splitting and joining

-- | @chunkRanges k n@ splits @[0, n)@ into at most @k@ half-open ranges of
-- as near the same length as they divide.
--
-- Static split rather than a work queue, for kb38's reason: the rows of a
-- bounded grid are not all the same cost --- an edge cell has fewer
-- neighbours than an interior one --- but the imbalance is a boundary
-- effect, so it shrinks as the grid grows, and a queue costs an atomic per
-- cell on every grid.
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

-- | Run the given actions on separate capabilities and wait for all of them.
--
-- 'forkOn' rather than 'forkIO', and exceptions re-raised in the caller
-- rather than printed to stderr and dropped: kb38's helper unchanged, for
-- kb38's reasons.
inParallel :: [IO a] -> IO [a]
inParallel acts = do
    caps <- getNumCapabilities
    slots <-
        forM (zip [0 :: Int ..] acts) $ \(i, act) -> do
            slot <- newEmptyMVar
            _ <- forkOn (i `mod` caps) (try (act >>= evaluate) >>= putMVar slot)
            pure slot
    results <- mapM takeMVar slots
    forM results $ either (throwIO @SomeException) pure
{-# INLINE inParallel #-}

-- * The three fills
--
-- Each takes the length and a function from index to element, so the two
-- kernels below differ only in that function and share every line of the
-- fill. That is deliberate: it is what lets a difference between the gather
-- kernel and the fold kernel be read as a property of the rule rather than
-- of the loop around it.

-- | Strict fill on one core: a mutable vector written with @$!@, so every
-- cell is in WHNF by the time the vector is frozen.
--
-- The @SeqStrict@ arm. No threads, no 'unsafePerformIO'; the only thing that
-- separates it from the library is when the rule runs.
fillSeqStrict :: VG.Vector v b => Int -> (Int -> b) -> v b
fillSeqStrict n at =
    runST $ do
        mv <- VGM.unsafeNew n
        let go !i
                | i >= n = pure ()
                | otherwise = (VGM.unsafeWrite mv i $! at i) >> go (i + 1)
        go 0
        VG.unsafeFreeze mv
{-# INLINE fillSeqStrict #-}

-- | 'fillSeqStrict' split across capabilities: one mutable vector, one
-- worker per chunk, disjoint slices, joined before the freeze.
--
-- @mult@ is chunks per capability. kb38 found the number did not matter for
-- the build (1 and 8 within 2%); whether that transfers to the run is one of
-- the questions this spike is here to answer, so it is a parameter.
fillPar :: VG.Vector v b => Int -> Int -> (Int -> b) -> v b
fillPar mult n at =
    unsafePerformIO $ do
        caps <- getNumCapabilities
        mv <- VGM.unsafeNew n
        let ranges = chunkRanges (max 1 (caps * mult)) n
            fill lo hi = go lo
              where
                go !i
                    | i >= hi = pure ()
                    | otherwise = (VGM.unsafeWrite mv i $! at i) >> go (i + 1)
        _ <- inParallel [fill lo hi | (lo, hi) <- ranges]
        VG.unsafeFreeze mv
{-# INLINE fillPar #-}

-- | The same split with no 'unsafePerformIO' in it: each chunk is its own
-- vector, the chunks are sparked, and the result is their concatenation.
--
-- What it buys is that it is a pure expression the compiler and the reader
-- can both see through. What it costs is the concatenation --- @n@ elements
-- copied and a second @n@ allocated --- and the weaker guarantee a spark
-- carries: a spark may fizzle, in which case the main thread does that
-- chunk itself, so this cannot be relied on to use the cores, only to be
-- allowed to.
fillSpark :: VG.Vector v b => Int -> Int -> (Int -> b) -> v b
fillSpark mult n at = VG.concat (sparked parts)
  where
    parts =
        [ deep (VG.generate (hi - lo) (\k -> at (lo + k)))
        | (lo, hi) <- chunkRanges (max 1 (numCapabilities * mult)) n
        ]
    -- What is sparked has to be what the consumer then demands, or the spark
    -- is unreachable the moment the consumer takes the other route and the
    -- RTS collects it: sparking @force c@ and handing on @c@ measured 6,185
    -- of 6,384 sparks GC'd and no speedup at all. So each element of 'parts'
    -- is already the deep-forcing thunk, and it is that same thunk that goes
    -- into the list --- a spark the main thread beats to it fizzles, which
    -- costs nothing, rather than being collected.
    sparked [] = []
    sparked (c:cs) = c `par` (let rest = sparked cs in rest `pseq` (c : rest))
    -- A chunk in WHNF is not a chunk that has been computed: an unboxed
    -- vector materialises when it is forced, but a boxed one is a vector of
    -- thunks, and a spark that stopped at WHNF would leave a boxed variant
    -- doing all its work on the main thread. Force the elements, then hand
    -- back the vector.
    deep c = VG.foldl' (\u x -> x `seq` u) () c `pseq` c
{-# INLINE fillSpark #-}

-- * The gather kernel: 'Data.Grid.Sized.Stencil.stencilGrid'

-- | The library's own path, transliterated.
--
-- Not the control --- the control is the library's 'stencilGrid' itself,
-- reached through its @.hi@ file the way a consumer reaches it. This is here
-- so that the variants below differ from something in this module by exactly
-- one thing.
--
-- The width and the table are bound once, strictly, outside the fill. That
-- is not tidiness: 'Stencil' is abstract, so this module reaches its fields
-- through the two accessors rather than through the pattern match the
-- library's own kernels use, and reading them inside the per-cell function
-- leaves both boxed and re-fetched once per cell. It measured 4.7x slower
-- than the library it is a transliteration of, and allocated four times as
-- much, until they were hoisted.
gridSeq ::
       forall v cs a b. (VG.Vector v a, VG.Vector v b)
    => Stencil cs
    -> (a -> [a] -> b)
    -> GridOf v cs a
    -> GridOf v cs b
gridSeq s f g = unsafeGridFromVector (VG.generate n at)
  where
    !w = stencilWidth s
    !tbl = stencilPositions s
    v = gridVector g
    n = VG.length v
    at i = f (VG.unsafeIndex v i) (gatherRow tbl w v i)
    {-# INLINE at #-}
{-# INLINABLE gridSeq #-}

-- | 'gridSeq' with nothing changed but the pragma: @INLINE@ where the library
-- says @INLINABLE@.
--
-- The arm that separates the two things the parallel variants below would
-- otherwise be credited with together. @INLINABLE@ exposes an unfolding for
-- GHC to /specialise/ on the types; it does not oblige GHC to /inline/ the
-- body. When it does not, the rule stays a lambda-bound variable inside the
-- fill, and every value that passes through it is boxed. Inlining makes the
-- rule known at the call site, and for the fold kernel that is worth about as
-- much as four cores are --- see 'foldSeqInline'.
gridSeqInline ::
       forall v cs a b. (VG.Vector v a, VG.Vector v b)
    => Stencil cs
    -> (a -> [a] -> b)
    -> GridOf v cs a
    -> GridOf v cs b
gridSeqInline s f g = unsafeGridFromVector (VG.generate n at)
  where
    !w = stencilWidth s
    !tbl = stencilPositions s
    v = gridVector g
    n = VG.length v
    at i = f (VG.unsafeIndex v i) (gatherRow tbl w v i)
    {-# INLINE at #-}
{-# INLINE gridSeqInline #-}

-- | 'gridSeq' with the rule forced where it is computed, and @INLINE@ as
-- 'gridSeqInline' explains.
gridSeqStrict ::
       forall v cs a b. (VG.Vector v a, VG.Vector v b)
    => Stencil cs
    -> (a -> [a] -> b)
    -> GridOf v cs a
    -> GridOf v cs b
gridSeqStrict s f g = unsafeGridFromVector (fillSeqStrict n at)
  where
    !w = stencilWidth s
    !tbl = stencilPositions s
    v = gridVector g
    n = VG.length v
    at i = f (VG.unsafeIndex v i) (gatherRow tbl w v i)
    {-# INLINE at #-}
{-# INLINE gridSeqStrict #-}

-- | 'gridSeqStrict' across capabilities.
gridPar ::
       forall v cs a b. (VG.Vector v a, VG.Vector v b)
    => Int
    -> Stencil cs
    -> (a -> [a] -> b)
    -> GridOf v cs a
    -> GridOf v cs b
gridPar mult s f g = unsafeGridFromVector (fillPar mult n at)
  where
    !w = stencilWidth s
    !tbl = stencilPositions s
    v = gridVector g
    n = VG.length v
    at i = f (VG.unsafeIndex v i) (gatherRow tbl w v i)
    {-# INLINE at #-}
{-# INLINE gridPar #-}

-- | 'gridPar' without the 'unsafePerformIO', through sparked chunks.
gridParSpark ::
       forall v cs a b. (VG.Vector v a, VG.Vector v b)
    => Int
    -> Stencil cs
    -> (a -> [a] -> b)
    -> GridOf v cs a
    -> GridOf v cs b
gridParSpark mult s f g = unsafeGridFromVector (fillSpark mult n at)
  where
    !w = stencilWidth s
    !tbl = stencilPositions s
    v = gridVector g
    n = VG.length v
    at i = f (VG.unsafeIndex v i) (gatherRow tbl w v i)
    {-# INLINE at #-}
{-# INLINE gridParSpark #-}

-- * The fold kernel: 'Data.Grid.Sized.Stencil.stencilFoldGrid'

-- | The library's own path, transliterated. See 'gridSeq', including why the
-- width and the table are bound where they are.
foldSeq ::
       forall v cs a b. (VG.Vector v a, VG.Vector v b)
    => Stencil cs
    -> (b -> a -> b)
    -> (a -> b)
    -> GridOf v cs a
    -> GridOf v cs b
foldSeq s step seed g = unsafeGridFromVector (VG.generate n at)
  where
    !w = stencilWidth s
    !tbl = stencilPositions s
    v = gridVector g
    n = VG.length v
    at i = foldRow' tbl w v i step (seed (VG.unsafeIndex v i))
    {-# INLINE at #-}
{-# INLINABLE foldSeq #-}

-- | 'foldSeq' with nothing changed but the pragma. See 'gridSeqInline'.
--
-- This is the arm that carries the spike's largest number, and it has no
-- threads in it. Left to specialise on types alone, the fold kernel's
-- accumulator is a boxed @Int@ that @step@ re-boxes once per neighbour, and
-- the neighbour it folds in is boxed on the way out of the unboxed vector as
-- well. Inlined, the whole row folds in registers.
foldSeqInline ::
       forall v cs a b. (VG.Vector v a, VG.Vector v b)
    => Stencil cs
    -> (b -> a -> b)
    -> (a -> b)
    -> GridOf v cs a
    -> GridOf v cs b
foldSeqInline s step seed g = unsafeGridFromVector (VG.generate n at)
  where
    !w = stencilWidth s
    !tbl = stencilPositions s
    v = gridVector g
    n = VG.length v
    at i = foldRow' tbl w v i step (seed (VG.unsafeIndex v i))
    {-# INLINE at #-}
{-# INLINE foldSeqInline #-}

-- | 'foldSeq' with each cell's accumulator forced where it is computed, and
-- @INLINE@ as 'foldSeqInline' explains.
foldSeqStrict ::
       forall v cs a b. (VG.Vector v a, VG.Vector v b)
    => Stencil cs
    -> (b -> a -> b)
    -> (a -> b)
    -> GridOf v cs a
    -> GridOf v cs b
foldSeqStrict s step seed g = unsafeGridFromVector (fillSeqStrict n at)
  where
    !w = stencilWidth s
    !tbl = stencilPositions s
    v = gridVector g
    n = VG.length v
    at i = foldRow' tbl w v i step (seed (VG.unsafeIndex v i))
    {-# INLINE at #-}
{-# INLINE foldSeqStrict #-}

-- | 'foldSeqStrict' across capabilities.
foldPar ::
       forall v cs a b. (VG.Vector v a, VG.Vector v b)
    => Int
    -> Stencil cs
    -> (b -> a -> b)
    -> (a -> b)
    -> GridOf v cs a
    -> GridOf v cs b
foldPar mult s step seed g = unsafeGridFromVector (fillPar mult n at)
  where
    !w = stencilWidth s
    !tbl = stencilPositions s
    v = gridVector g
    n = VG.length v
    at i = foldRow' tbl w v i step (seed (VG.unsafeIndex v i))
    {-# INLINE at #-}
{-# INLINE foldPar #-}

-- | 'foldPar' without the 'unsafePerformIO', through sparked chunks.
foldParSpark ::
       forall v cs a b. (VG.Vector v a, VG.Vector v b)
    => Int
    -> Stencil cs
    -> (b -> a -> b)
    -> (a -> b)
    -> GridOf v cs a
    -> GridOf v cs b
foldParSpark mult s step seed g = unsafeGridFromVector (fillSpark mult n at)
  where
    !w = stencilWidth s
    !tbl = stencilPositions s
    v = gridVector g
    n = VG.length v
    at i = foldRow' tbl w v i step (seed (VG.unsafeIndex v i))
    {-# INLINE at #-}
{-# INLINE foldParSpark #-}
