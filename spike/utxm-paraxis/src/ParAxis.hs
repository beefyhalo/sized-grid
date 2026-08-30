-- | Candidate walks for the two axis operations, for @sized-grid-utxm@.
--
-- Everything here computes what the library's
-- 'Data.Grid.Sized.mapAxis' and 'Data.Grid.Sized.scanAxis' compute, by a
-- different route. The geometry is an input: @axisSize@ and @stride@ arrive
-- as 'Int's exactly as the library's own @mapAxisStrided@ and
-- @scanAxisStrided@ take them, because deriving them is
-- 'Data.Grid.Sized.axisSizeAndStride' and is not what is in question.
--
-- The thing that makes this spike different from its two siblings is what the
-- unit of parallelism is. @sized-grid-kb38@ and @sized-grid-49hi@ both split
-- a per-cell fill, so the number of independent pieces was the number of
-- cells and the only question was whether there were enough of them. Here the
-- independent pieces are /fibres/, and there are @len \`quot\` axisSize@ of
-- them however many cells there are. A 4x22,500 grid has 90,000 cells and
-- four fibres along its inner axis, and four fibres do not fill eight cores.
-- So every threshold below is stated in fibres, not in cells, and the
-- benchmark carries a shape whose two axes disagree about that by four orders
-- of magnitude.
--
-- The axes of the experiment are three:
--
-- * /which axis/. @stride == 1@ is the innermost axis, whose fibres are
--   already contiguous; the library skips both the gather and the mutable
--   scatter for it. Any other axis is gather-apply-scatter over a strided
--   walk. These are two different programs in the library and they
--   parallelise differently, so no arm here is measured at only one of them.
-- * /how the chunk walks memory/. This one only exists for the scan, and it
--   is the whole finding. @scanAxisStrided@ deliberately does /not/ walk
--   fibres: it makes one in-order pass up the vector reading the element one
--   stride back out of the output. A parallel split that hands each worker a
--   set of fibres reinstates the walk the library rejected; a split that
--   hands each worker a contiguous /range of offsets/ and keeps the in-order
--   walk inside it does not. Both are here.
--
--   How much that costs turned out to depend entirely on whether the grid
--   fits in cache, which is why the benchmark grew a 32 MB shape. On a
--   300x300 grid the two walks measure the same to 2%; on a 2000x2000 grid
--   the fibre walk is 7.4x slower, and parallelising it across eight cores
--   still does not catch the in-order pass on one. Do not read any claim
--   here about stride and locality off a shape that fits in L2.
-- * /how many cores/, and at what chunk granularity.
--
-- The @Par@ variants are pure functions with 'unsafePerformIO' inside, on the
-- licence kb38's build variants took: the workers write disjoint slices of a
-- vector nothing else can see, join before it is frozen, and re-raise the
-- first exception any of them raised, so the result is a function of the
-- arguments alone. The @ParSpark@ variants express the same split with 'par'
-- and need no such licence; 49hi priced that purity at an extra @n@-element
-- copy, and one of the two cases here does not have to pay it.
module ParAxis
  ( -- * @mapAxis@, against 'Data.Grid.Sized.mapAxis'
    mapSeq,
    mapPar,
    mapParSpark,

    -- * @scanAxis@, against 'Data.Grid.Sized.scanAxis'
    scanSeq,
    scanSeqMut,
    scanSeqFreeze,
    scanSeqFibre,
    scanSeqStrip,
    scanParFibre,
    scanParStrip,
    scanParSpark,

    -- * Shared plumbing
    chunkRanges,
    fibreCount,
    segmentsOf,
  )
where

import Control.Concurrent (forkOn, getNumCapabilities)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, evaluate, throwIO, try)
import Control.Monad (forM, forM_, void)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import GHC.Conc (numCapabilities, par, pseq)
import System.IO.Unsafe (unsafePerformIO)

-- * Fibre geometry

--
-- A fibre along the target axis is @axisSize@ elements @stride@ apart. The
-- fibres starting in @[b, b + stride)@ cover the block
-- @[b, b + axisSize * stride)@ exactly, and the blocks tile the vector, so
-- every element belongs to exactly one fibre. That is the library's own
-- statement of the layout; what this module adds is a name for the fibres as
-- a numbered sequence, because a static split needs to index them.

-- | How many fibres a vector of the given length has along an axis of the
-- given size.
--
-- @len \`quot\` axisSize@, written through the block so it reads as what it
-- is: @stride@ fibres in each of @len \`quot\` block@ blocks.
--
-- This is the number that has to exceed the capability count, and it is not
-- the cell count. On a 300x300 grid it is 300 either way; on the 4x22,500
-- grid the benchmark carries it is 22,500 along one axis and 4 along the
-- other, for the same 90,000 cells.
fibreCount :: Int -> Int -> Int -> Int
fibreCount axisSize stride len = (len `quot` (axisSize * stride)) * stride
{-# INLINE fibreCount #-}

-- | Cut a half-open range of fibre indices into runs that lie inside one
-- block, as @(blockStart, o0, o1)@: the fibres based at
-- @[blockStart + o0, blockStart + o1)@.
--
-- A chunk of consecutive fibre indices is a chunk of consecutive /bases/
-- within a block, which is what makes the strip walk below contiguous. It
-- stops being consecutive at a block boundary, hence the cut.
--
-- The @quot@ and @rem@ are paid once per segment rather than once per fibre,
-- which is why the walks take segments rather than recomputing a base from an
-- index.
segmentsOf :: Int -> Int -> (Int, Int) -> [(Int, Int, Int)]
segmentsOf block stride (lo, hi) = go lo
  where
    go !j
      | j >= hi = []
      | otherwise =
          let o0 = j `rem` stride
              o1 = min stride (o0 + (hi - j))
           in ((j `quot` stride) * block, o0, o1) : go (j + (o1 - o0))

-- | One fibre, gathered: the @axisSize@ elements of @v@ that start at @base@
-- and sit @stride@ apart, copied out contiguous.
--
-- The library's @fibreAt@, byte for byte. Copied rather than imported --
-- @Data.Grid.Sized.Internal.Grid.Axis@ is an @other-module@ -- and a spike
-- that reimplemented it differently would be measuring a different program.
fibreAt :: (VG.Vector v a) => Int -> Int -> v a -> Int -> v a
fibreAt axisSize stride v base =
  VG.generate axisSize (\k -> VG.unsafeIndex v (base + k * stride))
{-# INLINE fibreAt #-}

-- | The library's @splitVectorBySize@ at the sizes this module uses it,
-- restated for the same reason.
splitBySize :: (VG.Vector v a) => Int -> v a -> [v a]
splitBySize n v = [VG.slice i (min n (len - i)) v | i <- [0, n .. len - 1]]
  where
    len = VG.length v
{-# INLINE splitBySize #-}

-- * Splitting and joining

-- | @chunkRanges k n@ splits @[0, n)@ into at most @k@ half-open ranges of as
-- near the same length as they divide.
--
-- kb38's helper unchanged. Static split rather than a work queue: every fibre
-- along one axis is the same length by construction, so unlike the stencil
-- grids kb38 and 49hi split there is not even a boundary effect to balance
-- here -- the pieces are exactly equal whenever the chunk count divides.
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
-- 'forkOn' rather than 'forkIO', and exceptions re-raised in the caller rather
-- than printed to stderr and dropped: kb38's helper unchanged, for kb38's
-- reasons.
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

-- | 'inParallel' at the shape 'stripBy' takes its runner, so that forking and
-- not forking are interchangeable there.
inParallel' :: [IO ()] -> IO ()
inParallel' acts = void (inParallel acts)
{-# INLINE inParallel' #-}

-- | The fibre ranges one call splits into, at @mult@ chunks per capability.
--
-- 'numCapabilities' rather than 'getNumCapabilities' so this is usable from
-- the pure sparked variants; the two agree unless a program calls
-- @setNumCapabilities@, which nothing here does.
fibreChunks :: Int -> Int -> [(Int, Int)]
fibreChunks mult = chunkRanges (max 1 (numCapabilities * mult))
{-# INLINE fibreChunks #-}

-- | Spark a list so that what is sparked is what the consumer then demands.
--
-- 49hi's helper, for 49hi's reason: sparking @force c@ and handing on @c@
-- leaves the spark unreachable the moment the consumer takes the other route,
-- and the RTS collects it. Each element here is already the deep-forcing
-- thunk and it is that same thunk that goes into the list, so a spark the main
-- thread beats to it fizzles rather than being collected.
sparked :: [a] -> [a]
sparked [] = []
sparked (c : cs) = c `par` (let rest = sparked cs in rest `pseq` (c : rest))

-- | A chunk in WHNF is not a chunk that has been computed: an unboxed vector
-- materialises when it is forced, but a boxed one is a vector of thunks, and a
-- spark that stopped at WHNF would leave a boxed variant doing all its work on
-- the main thread.
deep :: (VG.Vector v a) => v a -> v a
deep c = VG.foldl' (\u x -> x `seq` u) () c `pseq` c
{-# INLINE deep #-}

-- * mapAxis

-- | The library's @mapAxisStrided@, transliterated.
--
-- Not the control -- the control is the library's own 'Data.Grid.Sized.mapAxis'
-- reached through its @.hi@ file the way a consumer reaches it. This is here so
-- that the variants below differ from something in this module by exactly the
-- parallelism, and so the benchmark can show it and the real one measuring the
-- same.
mapSeq ::
  forall v x y.
  (VG.Vector v x, VG.Vector v y) =>
  Int ->
  Int ->
  (v x -> v y) ->
  v x ->
  v y
mapSeq axisSize stride f v
  | stride == 1 && axisSize > 0 = VG.concat (map f (splitBySize axisSize v))
  | otherwise =
      VG.create $ do
        out <- VGM.unsafeNew len
        let scatterFrom base =
              VG.imapM_ (\k -> VGM.unsafeWrite out (base + k * stride)) $
                f (fibreAt axisSize stride v base)
            fibresOf blockStart base
              | base >= blockStart + stride = pure ()
              | otherwise = scatterFrom base >> fibresOf blockStart (base + 1)
            blocks blockStart
              | blockStart >= len = pure ()
              | otherwise =
                  fibresOf blockStart blockStart >> blocks (blockStart + block)
        blocks 0
        pure out
  where
    len = VG.length v
    block = axisSize * stride
{-# INLINE mapSeq #-}

-- | 'mapSeq' split across capabilities by fibre: one mutable vector, one
-- worker per chunk of fibres, disjoint slices, joined before the freeze.
--
-- @f@ consumes a whole fibre, so there is no walk-order question here the way
-- there is for the scan: fibre-at-a-time is the only shape, and it is the
-- shape the sequential path already has. That makes this the honest
-- best case for parallelising an axis operation -- the split changes nothing
-- but which core runs which fibre.
--
-- Note what the two branches do to the memory the workers touch. On
-- @stride == 1@ a chunk of fibres is a contiguous run of the vector and the
-- workers never share a cache line except at the chunk edges. On any other
-- axis a chunk of fibres is a run of /bases/, so the workers write interleaved
-- words a stride apart and every cache line in the block is touched by the
-- one worker that owns that offset range -- still disjoint, still no false
-- sharing beyond the edges, but each worker's writes are strided rather than
-- sequential.
mapPar ::
  forall v x y.
  (VG.Vector v x, VG.Vector v y) =>
  -- | Chunks per capability.
  Int ->
  Int ->
  Int ->
  (v x -> v y) ->
  v x ->
  v y
mapPar mult axisSize stride f v
  | len == 0 || axisSize <= 0 = VG.empty
  | otherwise = unsafePerformIO $ do
      out <- VGM.unsafeNew len
      let writeFibreAt base =
            VG.imapM_ (\k -> VGM.unsafeWrite out (base + k * stride))
          -- The contiguous axis: fibre j is the slice at j * axisSize, so a
          -- chunk of fibres is a contiguous region of the output.
          contiguous (lo, hi) = goF lo
            where
              goF !j
                | j >= hi = pure ()
                | otherwise = do
                    let at = j * axisSize
                    VG.imapM_
                      (\k -> VGM.unsafeWrite out (at + k))
                      (f (VG.unsafeSlice at axisSize v))
                    goF (j + 1)
          -- Any other axis: gather the fibre, apply, scatter it back.
          strided r = forM_ (segmentsOf block stride r) $ \(bs, o0, o1) ->
            let goB !b
                  | b >= bs + o1 = pure ()
                  | otherwise =
                      writeFibreAt b (f (fibreAt axisSize stride v b))
                        >> goB (b + 1)
             in goB (bs + o0)
          work = if stride == 1 then contiguous else strided
      _ <- inParallel [work r | r <- fibreChunks mult nF]
      VG.unsafeFreeze out
  where
    len = VG.length v
    block = axisSize * stride
    nF = fibreCount axisSize stride len
{-# INLINE mapPar #-}

-- | The same split with no 'unsafePerformIO' in it.
--
-- On the contiguous axis this is the sequential expression with 'par' inserted
-- and /nothing else changed/: @mapSeq@'s fast path is already
-- @VG.concat (map f fibres)@, so sparking the very thunks that @concat@ is
-- about to demand costs not one extra allocation. That is the one place in
-- this project where purity has turned out to be free, and it is free because
-- the sequential implementation was already a concat.
--
-- On any other axis it is not free and cannot be: a chunk's outputs do not
-- occupy a contiguous region, so they have to be laid out somewhere and
-- scattered, which is an extra @n@-element vector and an extra @n@-element
-- copy on top of the strided scatter the sequential path already pays.
mapParSpark ::
  forall v x y.
  (VG.Vector v x, VG.Vector v y) =>
  Int ->
  Int ->
  Int ->
  (v x -> v y) ->
  v x ->
  v y
mapParSpark mult axisSize stride f v
  | len == 0 || axisSize <= 0 = VG.empty
  | stride == 1 = VG.concat (sparked (map (deep . f) (splitBySize axisSize v)))
  | otherwise = runST $ do
      out <- VGM.unsafeNew len
      forM_ (zip chunks parts) $ \((lo, hi), part) -> do
        let goSeg !j segs = case segs of
              [] -> pure ()
              ((bs, o0, o1) : rest) -> do
                let goB !b !k
                      | b >= bs + o1 = goSeg k rest
                      | otherwise = do
                          VG.imapM_
                            (\i -> VGM.unsafeWrite out (b + i * stride))
                            (VG.unsafeSlice (k * axisSize) axisSize part)
                          goB (b + 1) (k + 1)
                goB (bs + o0) j
        goSeg 0 (segmentsOf block stride (lo, hi))
      VG.unsafeFreeze out
  where
    len = VG.length v
    block = axisSize * stride
    nF = fibreCount axisSize stride len
    chunks = fibreChunks mult nF
    -- Each chunk's fibres, transformed and laid out contiguously in fibre
    -- order. This is the extra n elements the strided axis pays for purity.
    parts =
      sparked
        [ deep . VG.concat $
            [ f (fibreAt axisSize stride v b)
            | (bs, o0, o1) <- segmentsOf block stride r,
              b <- [bs + o0 .. bs + o1 - 1]
            ]
        | r <- chunks
        ]
{-# INLINE mapParSpark #-}

-- * scanAxis

-- | The library's @scanAxisStrided@, transliterated.
--
-- The control's twin, as 'mapSeq' is for the map. Note the shape of the
-- general branch, because every parallel variant below is judged against it:
-- it is /one in-order pass up the vector/. It never has a fibre in hand. The
-- first @stride@ elements of a block copy across, and every later element
-- combines the one a stride behind it, read back out of the output where it
-- was already written and already forced. Both the reads and the writes run
-- straight up memory.
scanSeq :: forall v a. (VG.Vector v a) => Int -> Int -> (a -> a -> a) -> v a -> v a
scanSeq axisSize stride f v
  | stride == 1 && axisSize > 0 =
      VG.concat (map (VG.scanl1' f) (splitBySize axisSize v))
  | otherwise =
      VG.create $ do
        out <- VGM.unsafeNew len
        let heads blockStart i
              | i >= blockStart + stride = pure ()
              | otherwise = do
                  let !a0 = VG.unsafeIndex v i
                  VGM.unsafeWrite out i a0
                  heads blockStart (i + 1)
            rest blockEnd i
              | i >= blockEnd = pure ()
              | otherwise = do
                  prev <- VGM.unsafeRead out (i - stride)
                  let !acc = f prev (VG.unsafeIndex v i)
                  VGM.unsafeWrite out i acc
                  rest blockEnd (i + 1)
            blocks blockStart
              | blockStart >= len = pure ()
              | otherwise = do
                  heads blockStart blockStart
                  rest (blockStart + block) (blockStart + stride)
                  blocks (blockStart + block)
        blocks 0
        pure out
  where
    len = VG.length v
    block = axisSize * stride
{-# INLINE scanSeq #-}

-- | 'scanSeq' with the contiguous branch scanning straight into the output
-- instead of concatenating per-fibre vectors. One core, no threads.
--
-- The arm that keeps a sequential saving from being credited to the cores.
-- @VG.concat (map (VG.scanl1' f) ...)@ allocates a vector per fibre and then
-- allocates the result and copies every element into it: @2n@ words where a
-- direct fill needs @n@. The general branch already fills directly, so this
-- differs from 'scanSeq' on the innermost axis and nowhere else -- which is
-- exactly why it has to be measured before the parallel innermost-axis arms
-- are, since they fill directly too and would otherwise pocket this.
scanSeqMut :: forall v a. (VG.Vector v a) => Int -> Int -> (a -> a -> a) -> v a -> v a
scanSeqMut axisSize stride f v
  | stride /= 1 || axisSize <= 0 = scanSeq axisSize stride f v
  | otherwise =
      VG.create $ do
        out <- VGM.unsafeNew len
        let runs !start
              | start >= len = pure ()
              | otherwise = do
                  let !a0 = VG.unsafeIndex v start
                  VGM.unsafeWrite out start a0
                  let end = start + axisSize
                      go !i !acc
                        | i >= end = pure ()
                        | otherwise = do
                            let !acc' = f acc (VG.unsafeIndex v i)
                            VGM.unsafeWrite out i acc'
                            go (i + 1) acc'
                  go (start + 1) a0
                  runs end
        runs 0
        pure out
  where
    len = VG.length v
{-# INLINE scanSeqMut #-}

-- | 'scanSeqMut' with @'VG.create'@ replaced by @'runST'@ over an explicit
-- @'VGM.unsafeNew'@ and @'VG.unsafeFreeze'@, and nothing else changed.
--
-- The arm that names the 2x this spike did not go looking for. 'scanSeqMut'
-- and 'scanSeqStrip' run line-for-line the same loop and allocate the same
-- 703 KB, and 'scanSeqStrip' measured twice as fast; the chunk count turned
-- out not to be the cause ('scanSeqStrip' at one chunk is a single sequential
-- loop and measures the same as at 32), which left the way the result vector
-- is constructed as the only thing separating them. This arm holds the loop
-- fixed and changes only that, and it stays pure -- no 'unsafePerformIO' --
-- so that whatever it shows is something the library could adopt directly.
scanSeqFreeze :: forall v a. (VG.Vector v a) => Int -> Int -> (a -> a -> a) -> v a -> v a
scanSeqFreeze axisSize stride f v
  | stride /= 1 || axisSize <= 0 = scanSeq axisSize stride f v
  | otherwise = runST $ do
      out <- VGM.unsafeNew len
      let runs !start
            | start >= len = pure ()
            | otherwise = do
                let !a0 = VG.unsafeIndex v start
                VGM.unsafeWrite out start a0
                let end = start + axisSize
                    go !i !acc
                      | i >= end = pure ()
                      | otherwise = do
                          let !acc' = f acc (VG.unsafeIndex v i)
                          VGM.unsafeWrite out i acc'
                          go (i + 1) acc'
                go (start + 1) a0
                runs end
      runs 0
      VG.unsafeFreeze out
  where
    len = VG.length v
{-# INLINE scanSeqFreeze #-}

-- | The scan done fibre at a time, on one core: for each base, walk that
-- fibre end to end carrying the running total in an argument.
--
-- Nothing parallel about it. It is here because it is the shape a fibre-split
-- forces, and without it the fibre-split parallel arm would be charged for
-- the walk and credited to the threads at the same time. The library's
-- Haddock on @scanAxisStrided@ records having written this first and measured
-- it slower -- 679 vs 575 us boxed and 254 vs 68 us unboxed on a 300x300 grid
-- -- so 'scanParFibre' begins somewhere around 3.7x in the hole and has to
-- win that back before it wins anything.
--
-- It reads and writes @axisSize@ words a stride apart per fibre, so on any
-- axis but the innermost it touches a fresh cache line for all but the last
-- of them and reuses none. 'scanSeq' touches each line once and uses all of
-- it.
scanSeqFibre :: forall v a. (VG.Vector v a) => Int -> Int -> (a -> a -> a) -> v a -> v a
scanSeqFibre axisSize stride f v
  | len == 0 || axisSize <= 0 = VG.empty
  | otherwise =
      VG.create $ do
        out <- VGM.unsafeNew len
        scanFibres f axisSize stride v out (0, fibreCount axisSize stride len) block
        pure out
  where
    len = VG.length v
    block = axisSize * stride
{-# INLINE scanSeqFibre #-}

-- | Scan every fibre in a range of fibre indices, one fibre at a time.
--
-- Shared by 'scanSeqFibre' and 'scanParFibre' so that the only difference
-- between them is which thread calls it, which is what makes the pair
-- readable as the cost of the threads alone.
--
-- Polymorphic in the monad because those two reach a mutable vector by
-- different routes -- 'VG.create' in @ST@ and 'unsafePerformIO' in @IO@ --
-- and writing the loop twice would leave the two arms free to drift.
scanFibres ::
  (PrimMonad m, VG.Vector v a) =>
  (a -> a -> a) ->
  Int ->
  Int ->
  v a ->
  VG.Mutable v (PrimState m) a ->
  (Int, Int) ->
  Int ->
  m ()
scanFibres f axisSize stride v out r block =
  forM_ (segmentsOf block stride r) $ \(bs, o0, o1) ->
    let fibre !base = do
          let !a0 = VG.unsafeIndex v base
          VGM.unsafeWrite out base a0
          go (base + stride) (axisSize - 1) a0
          where
            go !i !n !acc
              | n <= 0 = pure ()
              | otherwise = do
                  let !acc' = f acc (VG.unsafeIndex v i)
                  VGM.unsafeWrite out i acc'
                  go (i + stride) (n - 1) acc'
        goB !b
          | b >= bs + o1 = pure ()
          | otherwise = fibre b >> goB (b + 1)
     in goB (bs + o0)
{-# INLINE scanFibres #-}

-- | 'scanSeqFibre' split across capabilities: fibres are independent, so a
-- chunk of them is a unit of work with no communication in it at all.
--
-- The obvious parallelisation, and the one that does not pay, for the reason
-- 'scanSeqFibre' exists to show: it is a parallel version of the walk the
-- library measured and rejected.
scanParFibre ::
  forall v a.
  (VG.Vector v a) =>
  Int ->
  Int ->
  Int ->
  (a -> a -> a) ->
  v a ->
  v a
scanParFibre mult axisSize stride f v
  | len == 0 || axisSize <= 0 = VG.empty
  | otherwise = unsafePerformIO $ do
      out <- VGM.unsafeNew len
      _ <-
        inParallel
          [scanFibres f axisSize stride v out r block | r <- fibreChunks mult nF]
      VG.unsafeFreeze out
  where
    len = VG.length v
    block = axisSize * stride
    nF = fibreCount axisSize stride len
{-# INLINE scanParFibre #-}

-- | The scan split across capabilities /without/ giving up the in-order walk:
-- each worker owns a contiguous range of fibre bases and sweeps it row by row.
--
-- The point of the spike. A worker's chunk is a range of offsets
-- @[o0, o1)@ within a block, and it walks
-- @blockStart + k * stride + o0 .. blockStart + k * stride + o1@ for
-- @k = 0, 1, ... axisSize - 1@. Every row of that is a contiguous run of
-- @o1 - o0@ words -- the same sequential access 'scanSeq' makes, over a
-- narrower strip -- and the dependency it needs, the element one stride back,
-- is the same word of the previous row, which this worker wrote itself on the
-- previous iteration. So the split needs no communication and gives up
-- nothing.
--
-- On the innermost axis there are no offsets to strip: @stride == 1@ means one
-- fibre per block, so a chunk is a run of whole contiguous fibres and each is
-- scanned end to end. That branch matches 'scanSeqMut', not 'scanSeq'.
--
-- The strip width is @(o1 - o0)@ and it falls as the chunk count rises. Once
-- it drops below a cache line -- eight 'Int's -- neighbouring workers are
-- reading and writing the same lines, and the split stops being free. That is
-- the one reason to expect the chunk multiplier to matter here where kb38 and
-- 49hi both found it did not, and it is measured.
scanParStrip ::
  forall v a.
  (VG.Vector v a) =>
  Int ->
  Int ->
  Int ->
  (a -> a -> a) ->
  v a ->
  v a
scanParStrip = stripBy inParallel'
{-# INLINE scanParStrip #-}

-- | 'scanParStrip''s code with the chunks run one after another on the calling
-- thread instead of forked.
--
-- The control that separates the strip /split/ from the strip /threads/, and
-- it earns its place: at @-N1@ the strip arm measured 1.4x faster than
-- 'scanSeqMut' on the innermost axis, where there is no parallelism to be had
-- and the two loops are line-for-line the same. Something other than the
-- threads was paying for that, and without this arm there is no way to say
-- what share of any parallel number is really parallel.
scanSeqStrip ::
  forall v a.
  (VG.Vector v a) =>
  Int ->
  Int ->
  Int ->
  (a -> a -> a) ->
  v a ->
  v a
scanSeqStrip = stripBy sequence_
{-# INLINE scanSeqStrip #-}

-- | The strip walk, over a runner that decides whether the chunks are forked.
stripBy ::
  forall v a.
  (VG.Vector v a) =>
  ([IO ()] -> IO ()) ->
  Int ->
  Int ->
  Int ->
  (a -> a -> a) ->
  v a ->
  v a
stripBy runner mult axisSize stride f v
  | len == 0 || axisSize <= 0 = VG.empty
  | otherwise = unsafePerformIO $ do
      out <- VGM.unsafeNew len
      let -- stride == 1: a chunk is a run of whole contiguous fibres.
          contiguous (lo, hi) = goF (lo * axisSize)
            where
              stop = hi * axisSize
              goF !start
                | start >= stop = pure ()
                | otherwise = do
                    let !a0 = VG.unsafeIndex v start
                    VGM.unsafeWrite out start a0
                    let end = start + axisSize
                        go !i !acc
                          | i >= end = pure ()
                          | otherwise = do
                              let !acc' = f acc (VG.unsafeIndex v i)
                              VGM.unsafeWrite out i acc'
                              go (i + 1) acc'
                    go (start + 1) a0
                    goF end
          -- Otherwise: sweep the strip row by row, in order within each row.
          strip r = forM_ (segmentsOf block stride r) $ \(bs, o0, o1) -> do
            let heads !i
                  | i >= bs + o1 = pure ()
                  | otherwise = do
                      let !a0 = VG.unsafeIndex v i
                      VGM.unsafeWrite out i a0
                      heads (i + 1)
                row !i !stop
                  | i >= stop = pure ()
                  | otherwise = do
                      prev <- VGM.unsafeRead out (i - stride)
                      let !acc = f prev (VG.unsafeIndex v i)
                      VGM.unsafeWrite out i acc
                      row (i + 1) stop
                rows !k !at
                  | k >= axisSize = pure ()
                  | otherwise = row (at + o0) (at + o1) >> rows (k + 1) (at + stride)
            heads (bs + o0)
            rows 1 (bs + stride)
          work = if stride == 1 then contiguous else strip
      runner [work r | r <- fibreChunks mult nF]
      VG.unsafeFreeze out
  where
    len = VG.length v
    block = axisSize * stride
    nF = fibreCount axisSize stride len
{-# INLINE stripBy #-}

-- | The scan split with no 'unsafePerformIO' in it.
--
-- On the innermost axis this is 'scanSeq''s own expression with 'par'
-- inserted and nothing else changed, so it is free in the same way
-- 'mapParSpark' is -- but note which expression: it is free relative to
-- 'scanSeq', which is the arm 'scanSeqMut' shows is already paying @2n@ for
-- the concat. Purity being free here and the concat costing a copy are the
-- same fact seen from two sides.
--
-- On any other axis a chunk's outputs are strided, so they are laid out
-- contiguously per chunk and scattered afterwards: an extra @n@ and an extra
-- copy, the price 49hi found sparks charge.
scanParSpark ::
  forall v a.
  (VG.Vector v a) =>
  Int ->
  Int ->
  Int ->
  (a -> a -> a) ->
  v a ->
  v a
scanParSpark mult axisSize stride f v
  | len == 0 || axisSize <= 0 = VG.empty
  | stride == 1 =
      VG.concat (sparked (map (deep . VG.scanl1' f) (splitBySize axisSize v)))
  | otherwise = runST $ do
      out <- VGM.unsafeNew len
      forM_ (zip chunks parts) $ \(r, part) -> do
        let goSeg !k segs = case segs of
              [] -> pure ()
              ((bs, o0, o1) : more) -> do
                let goB !b !j
                      | b >= bs + o1 = goSeg j more
                      | otherwise = do
                          let src = VG.unsafeSlice (j * axisSize) axisSize part
                          VG.imapM_ (\i -> VGM.unsafeWrite out (b + i * stride)) src
                          goB (b + 1) (j + 1)
                goB (bs + o0) k
        goSeg 0 (segmentsOf block stride r)
      VG.unsafeFreeze out
  where
    len = VG.length v
    block = axisSize * stride
    nF = fibreCount axisSize stride len
    chunks = fibreChunks mult nF
    parts =
      sparked
        [ deep . VG.concat $
            [ VG.scanl1' f (fibreAt axisSize stride v b)
            | (bs, o0, o1) <- segmentsOf block stride r,
              b <- [bs + o0 .. bs + o1 - 1]
            ]
        | r <- chunks
        ]
{-# INLINE scanParSpark #-}
