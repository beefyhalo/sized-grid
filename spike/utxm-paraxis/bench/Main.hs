{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Axis-operation benchmarks for @sized-grid-utxm@: what walking one named
-- axis of a grid costs, sequentially and on several cores.
--
-- Two operations, because the library has two and they are not the same
-- problem. 'mapAxis' hands a whole fibre to a function, so its fibres are
-- independent and its sequential path already walks them one at a time --
-- parallelising it changes nothing but which core runs which fibre.
-- 'scanAxis' has a serial dependency /inside/ each fibre and independence
-- /between/ them, and its sequential path exploits the layout by never
-- holding a fibre at all. Those two facts pull in opposite directions and the
-- whole question is which wins.
--
-- Every shape is measured along axis 0 and axis 1, because in the library
-- those are two different programs: axis 1 of a 2-D grid has @stride == 1@
-- and takes a contiguous fast path, and axis 0 does not.
--
-- 'Thin' is the shape that makes this spike's point. It has the same 90,000
-- cells as 'Big', but 22,500 fibres along one axis and four along the other.
-- The unit of parallelism here is the fibre, so a grid can be large and still
-- have nothing to split.
--
-- Every variant is checked before anything is measured. 'checks' compares
-- each one against the library's own answer on both vector representations,
-- both axes, and an order-sensitive operator, and exits non-zero on the first
-- disagreement, so a variant that is fast because it scrambled a fibre fails
-- rather than reports.
module Main (main) where

import Control.Concurrent (getNumCapabilities)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (forM_, unless, void)
import Data.Grid.Sized
import Data.Grid.Sized.Unboxed (UGrid)
import Data.Grid.Sized.Unsafe (unsafeGridFromVector)
import Data.Kind (Type)
import Data.Vector.Generic qualified as VG
import ParAxis
import System.Exit (exitFailure)
import Test.Tasty.Bench

-- * The shapes

--
-- Names and sizes taken from @bench\/Main.hs@ in the library and from
-- @spike\/49hi-parexec@, so a number here can be read next to one there. The
-- smaller shapes are there to find the crossover, which is the only way "when
-- overhead dominates" gets an answer rather than an assertion.

-- | 12 cells. Below any plausible threshold: here the fork is the whole cost.
type Tiny = '[Clamped 3, Clamped 4]

-- | 400 and 1,024 cells, either side of where kb38 put the build's crossover.
type Small20 = '[Clamped 20, Clamped 20]

type Small32 = '[Clamped 32, Clamped 32]

-- | 2,500 cells: the automaton step the library's stencil Haddock quotes.
type Step = '[Clamped 50, Clamped 50]

-- | 10,000 cells.
type Mid = '[Clamped 100, Clamped 100]

-- | 40,000 cells.
type Wide = '[Clamped 200, Clamped 200]

-- | 90,000 cells, a real consumer's working size, and the shape the library's
-- own summed-area-table benchmark uses.
type Big = '[Clamped 300, Clamped 300]

-- | 90,000 cells again, but 4 x 22,500 rather than 300 x 300.
--
-- The shape that separates cells from fibres. Along axis 1 (@stride == 1@)
-- this has four fibres of 22,500 elements: four units of work, however many
-- cores are asked for, and a static split can use no more than four of them.
-- Along axis 0 it has 22,500 fibres of four elements, which is the opposite
-- extreme -- abundant parallelism, and so little work per fibre that the
-- per-fibre overhead is the whole cost.
type Thin = '[Clamped 4, Clamped 22500]

-- | 4,000,000 cells -- 32 MB of 'Int' -- which is the only shape here that
-- does not fit in cache.
--
-- This machine's performance cluster has a 16 MB L2 shared by its four cores,
-- so every other shape in this file is resident: 'Big' is 720 KB and never
-- misses, which is why a strided walk and an in-order walk measure the same on
-- it however different their access patterns are. Any claim about stride and
-- cache has to be made somewhere the cache is actually missing, and this is
-- that shape. Unboxed only -- the boxed equivalent is a 4,000,000-pointer
-- vector plus 4,000,000 boxes, and would measure the allocator.
type Huge = '[Clamped 2000, Clamped 2000]

-- | 90,000 cells in three dimensions, so that a middle axis can be measured.
--
-- Axis 1 here has @stride == 100@ and @block == 3,000@, so the vector holds
-- 30 blocks of 30 fibres each: the only shape in the set where a chunk of
-- fibres can span a block boundary, which is the case 'segmentsOf' exists for
-- and which the 2-D shapes never exercise.
type Cube = '[Clamped 30, Clamped 30, Clamped 100]

-- * The grids

boxed :: forall (cs :: [Type]). (IsCoordList cs, AllSizedKnown cs) => Grid cs Int
boxed = tabulateGrid coordPosition

unboxed ::
  forall (cs :: [Type]).
  (IsCoordList cs, AllSizedKnown cs) =>
  UGrid cs Int
unboxed = tabulateGrid coordPosition

-- * Reaching the geometry

-- | The @(axisSize, stride)@ pair the library would compute for this axis of
-- this shape.
--
-- The kernels in "ParAxis" take the pair as two 'Int's, exactly as the
-- library's own @mapAxisStrided@ and @scanAxisStrided@ do. Deriving it is
-- 'axisSizeAndStride' and is not what is in question here, so the benchmark
-- asks the library for it rather than writing the literals out -- which also
-- means an arm cannot silently be measured at a geometry the library would
-- never hand it.
geomOf ::
  forall v cs c a.
  forall n ->
  (MapAxis n cs c) =>
  GridOf v cs a ->
  (Int, Int)
geomOf n _ = axisSizeAndStride @n @cs @c
{-# INLINE geomOf #-}

-- | Run a vector-level kernel as a grid-level one.
--
-- Safe for exactly the reason 'unsafeGridFromVector' asks: every kernel in
-- "ParAxis" is length-preserving by construction, so the size invariant that
-- held on the way in holds on the way out.
onVec ::
  forall v cs.
  (VG.Vector v Int) =>
  (v Int -> v Int) ->
  GridOf v cs Int ->
  GridOf v cs Int
onVec f = unsafeGridFromVector . f . gridVector
{-# INLINE onVec #-}

-- * The workloads

-- | The cheapest length-preserving fibre function there is.
--
-- Deliberately trivial. A fibre function expensive enough to dominate would
-- make every parallel arm look good and would say nothing about the walk, so
-- the default workload is the one where the walk is the whole cost.
bumpGrid :: (VG.Vector v Int) => GridOf v cs Int -> GridOf v cs Int
bumpGrid = mapGrid (+ 1)

bumpVec :: (VG.Vector v Int) => v Int -> v Int
bumpVec = VG.map (+ 1)

-- | An order-sensitive fibre function, for the checks.
--
-- A per-element bump cannot tell a variant that reversed a fibre, or that
-- gathered it from the wrong base, from one that did not. Reversal can.
revGrid :: (VG.Vector v Int) => GridOf v cs Int -> GridOf v cs Int
revGrid = onVec VG.reverse

revVec :: (VG.Vector v Int) => v Int -> v Int
revVec = VG.reverse

-- | A moderately priced fibre function: the prefix scan itself.
--
-- @'mapAxis' n ('scanl1Grid' f)@ is what 'scanAxis' is documented to equal,
-- so measuring 'mapAxis' at this function puts the two operations on one
-- scale -- it is the same answer computed by the route 'scanAxis' exists to
-- avoid.
scanlGrid :: (VG.Vector v Int) => GridOf v cs Int -> GridOf v cs Int
scanlGrid = scanl1Grid (+)

scanlVec :: (VG.Vector v Int) => v Int -> v Int
scanlVec = VG.scanl1' (+)

-- | The scan operator the timing arms use.
plus :: Int -> Int -> Int
plus = (+)

-- | An order- and associativity-sensitive scan operator, for the checks.
horner :: Int -> Int -> Int
horner a x = a * 3 + x

-- * Checking

-- | Every variant against the library's own answer, for one grid and one
-- axis.
--
-- The comparison is on the result vector rather than the grid so that a
-- mismatch prints as two lists of 'Int' rather than needing an 'Eq' instance
-- the library does not owe anyone.
verify ::
  forall v cs c.
  forall n ->
  (MapAxis n cs c, VG.Vector v Int) =>
  String ->
  GridOf v cs Int ->
  IO Bool
verify n label g = do
  let (as, st) = geomOf n g
      v = gridVector g
      mapArms =
        [ (nm ++ "/" ++ wl, VG.toList (k m as st fv v) == want)
        | (wl, gf, fv) <- workloads,
          let want = VG.toList (gridVector (mapAxis n gf g)),
          (nm, k, m) <- mapVariants
        ]
      scanArms =
        [ (nm ++ "/" ++ wl, VG.toList (k m as st sf v) == want)
        | (wl, sf) <- [("plus", plus), ("horner", horner)],
          let want = VG.toList (gridVector (scanAxis n sf g)),
          (nm, k, m) <- scanVariants
        ]
      results = mapArms ++ scanArms
  forM_ results $ \(nm, ok) ->
    unless ok $ putStrLn ("MISMATCH: " ++ label ++ " / " ++ nm)
  pure (all snd results)
  where
    workloads =
      [ ("bump", bumpGrid, bumpVec),
        ("reverse", revGrid, revVec),
        ("scanl1", scanlGrid, scanlVec)
      ]
    -- Every parallel variant is checked at three chunk multipliers, because
    -- the multiplier is what decides whether a chunk spans a block boundary
    -- and whether a strip is narrower than a fibre count. Those are the two
    -- places the index arithmetic can be wrong.
    mapVariants =
      ("mapSeq", \_ a s f -> mapSeq a s f, 0)
        : [ (nm ++ " mult=" ++ show m, k, m)
          | (nm, k) <- [("mapPar", mapPar), ("mapParSpark", mapParSpark)],
            m <- mults
          ]
    scanVariants =
      [ ("scanSeq", \_ a s f -> scanSeq a s f, 0),
        ("scanSeqMut", \_ a s f -> scanSeqMut a s f, 0),
        ("scanSeqFreeze", \_ a s f -> scanSeqFreeze a s f, 0),
        ("scanSeqFibre", \_ a s f -> scanSeqFibre a s f, 0)
      ]
        ++ [("scanSeqStrip mult=" ++ show m, scanSeqStrip, m) | m <- mults]
        ++ [ (nm ++ " mult=" ++ show m, k, m)
           | (nm, k) <-
               [ ("scanParFibre", scanParFibre),
                 ("scanParStrip", scanParStrip),
                 ("scanParSpark", scanParSpark)
               ],
             m <- mults
           ]
    mults = [1, 2, 8, 32 :: Int]

-- | The checks, over both representations, both axes, and every shape whose
-- geometry differs from the others in a way the index arithmetic can notice.
checks :: IO Bool
checks =
  fmap and . sequence $
    [ verify 0 "Tiny boxed axis0" (boxed @Tiny),
      verify 1 "Tiny boxed axis1" (boxed @Tiny),
      verify 0 "Tiny unboxed axis0" (unboxed @Tiny),
      verify 1 "Tiny unboxed axis1" (unboxed @Tiny),
      verify 0 "Small20 boxed axis0" (boxed @Small20),
      verify 1 "Small20 boxed axis1" (boxed @Small20),
      verify 0 "Step unboxed axis0" (unboxed @Step),
      verify 1 "Step unboxed axis1" (unboxed @Step),
      verify 0 "Mid boxed axis0" (boxed @Mid),
      verify 1 "Mid boxed axis1" (boxed @Mid),
      -- 4 x 22,500: four fibres one way, 22,500 the other.
      verify 0 "Thin unboxed axis0" (unboxed @Thin),
      verify 1 "Thin unboxed axis1" (unboxed @Thin),
      -- Three dimensions, so a chunk of fibres can span a block boundary --
      -- the case 'segmentsOf' exists for.
      verify 0 "Cube unboxed axis0" (unboxed @Cube),
      verify 1 "Cube unboxed axis1" (unboxed @Cube),
      verify 2 "Cube unboxed axis2" (unboxed @Cube),
      verify 1 "Cube boxed axis1" (boxed @Cube)
    ]

-- * Measuring

-- | The 'mapAxis' arms for one grid and one axis.
--
-- Four, and each adds exactly one thing to the one before it: the library
-- itself, its transliteration, the cores, and the cores without
-- 'unsafePerformIO'.
--
-- 'nf' rather than 'whnf' throughout. On a boxed grid the library's fast path
-- is a 'VG.concat' of per-fibre vectors of thunks, so under 'whnf' the
-- sequential arms would appear to do no work at all and the parallel ones --
-- which force in the worker -- would appear to do all of it.
mapGroup ::
  forall v cs c.
  forall n ->
  (MapAxis n cs c, VG.Vector v Int, NFData (GridOf v cs Int)) =>
  String ->
  Int ->
  (GridOf v '[c] Int -> GridOf v '[c] Int) ->
  (v Int -> v Int) ->
  GridOf v cs Int ->
  Benchmark
mapGroup n label mult gf fv g =
  bgroup
    label
    [ bench "mapAxis (library)" $ nf (mapAxis n gf) g,
      bench "mapSeq" $ nf (onVec (mapSeq as st fv)) g,
      bench "mapPar" $ nf (onVec (mapPar mult as st fv)) g,
      bench "mapParSpark" $ nf (onVec (mapParSpark mult as st fv)) g
    ]
  where
    (as, st) = geomOf n g
-- Inlined at each call site, and that is load-bearing rather than tidy: this
-- function is polymorphic in the vector, so left out of line every variant it
-- names is called through a dictionary and none of them specialise. That is
-- not what a consumer's code looks like, and 49hi measured it moving the
-- answer.
{-# INLINE mapGroup #-}

-- | The 'scanAxis' arms for one grid and one axis.
--
-- Seven, and the order is the argument. @scanAxis@ is the library;
-- @scanSeq@ is its transliteration; @scanSeqMut@ is a sequential saving on
-- the contiguous axis that the parallel arms would otherwise pocket;
-- @scanSeqFibre@ is the walk a fibre split forces, on one core, so the walk
-- and the threads are charged separately; @scanSeqStrip@ is the strip split's
-- code run without forking, so the split and the threads are charged
-- separately too; @scanParFibre@ is the fibre walk on many
-- cores; @scanParStrip@ is many cores /without/ giving up the in-order walk;
-- @scanParSpark@ is the strip split's purity price.
scanGroup ::
  forall v cs c.
  forall n ->
  (MapAxis n cs c, VG.Vector v Int, NFData (GridOf v cs Int)) =>
  String ->
  Int ->
  GridOf v cs Int ->
  Benchmark
scanGroup n label mult g =
  bgroup
    label
    [ bench "scanAxis (library)" $ nf (scanAxis n plus) g,
      bench "scanSeq" $ nf (onVec (scanSeq as st plus)) g,
      bench "scanSeqMut" $ nf (onVec (scanSeqMut as st plus)) g,
      bench "scanSeqFreeze" $ nf (onVec (scanSeqFreeze as st plus)) g,
      bench "scanSeqFibre" $ nf (onVec (scanSeqFibre as st plus)) g,
      bench "scanSeqStrip" $ nf (onVec (scanSeqStrip mult as st plus)) g,
      bench "scanParFibre" $ nf (onVec (scanParFibre mult as st plus)) g,
      bench "scanParStrip" $ nf (onVec (scanParStrip mult as st plus)) g,
      bench "scanParSpark" $ nf (onVec (scanParSpark mult as st plus)) g
    ]
  where
    (as, st) = geomOf n g
{-# INLINE scanGroup #-}

-- | The same split at several chunk counts.
--
-- kb38 and 49hi both found the chunk multiplier did not matter once there was
-- enough work to hide the dispatch. 'scanParStrip' has a reason they did not
-- to care: its chunk is a /strip/ of the block @(o1 - o0)@ words wide, and
-- that width falls as the chunk count rises. Once it drops below a cache line
-- -- eight 'Int's -- neighbouring workers are writing the same lines, and a
-- split that was free stops being free. On 'Big' along axis 0 there are 300
-- fibres, so at four capabilities @mult = 8@ gives strips about 9 words wide
-- and @mult = 32@ about 2, which is where the effect has to show if it is
-- there at all.
chunkGroup ::
  forall v cs c.
  forall n ->
  (MapAxis n cs c, VG.Vector v Int, NFData (GridOf v cs Int)) =>
  String ->
  GridOf v cs Int ->
  Benchmark
chunkGroup n label g =
  bgroup label $
    [ bench ("scanSeqStrip mult=" ++ show m) $
        nf (onVec (scanSeqStrip m as st plus)) g
    | m <- mults
    ]
      ++ [ bench ("scanParStrip mult=" ++ show m) $
             nf (onVec (scanParStrip m as st plus)) g
         | m <- mults
         ]
      ++ [ bench ("scanParFibre mult=" ++ show m) $
             nf (onVec (scanParFibre m as st plus)) g
         | m <- mults
         ]
      ++ [ bench ("mapPar mult=" ++ show m) $
             nf (onVec (mapPar m as st bumpVec)) g
         | m <- mults
         ]
  where
    (as, st) = geomOf n g
    mults = [1, 2, 8, 32 :: Int]
{-# INLINE chunkGroup #-}

-- | Force every grid before a single benchmark runs.
--
-- Without this the first arm of each group pays to materialise its input, and
-- the first arm of each group is the library control. A boxed 'Grid' built by
-- 'tabulateGrid' is a vector of thunks until something forces it, and the
-- first benchmark to 'nf' a result over it is what does. 49hi measured that
-- charging the library 1.5x its own transliteration purely for being listed
-- first.
warm :: IO ()
warm = do
  grid (boxed @Tiny)
  grid (unboxed @Tiny)
  grid (boxed @Small20)
  grid (boxed @Small32)
  grid (boxed @Step)
  grid (unboxed @Step)
  grid (boxed @Mid)
  grid (unboxed @Mid)
  grid (unboxed @Wide)
  grid (boxed @Big)
  grid (unboxed @Big)
  grid (boxed @Thin)
  grid (unboxed @Thin)
  grid (boxed @Cube)
  grid (unboxed @Cube)
  grid (unboxed @Huge)
  where
    grid :: (NFData a) => a -> IO ()
    grid = void . evaluate . force

main :: IO ()
main = do
  ok <- checks
  unless ok $ do
    putStrLn "variants disagree with the library; not measuring"
    exitFailure
  warm
  caps <- getNumCapabilities
  putStrLn ("capabilities: " ++ show caps)
  let m = 2
  defaultMain
    [ -- Finding the crossover, on the strided axis and the cheapest workload.
      mapGroup 0 "00,012 cells (Tiny) axis0 boxed  map" m bumpGrid bumpVec (boxed @Tiny),
      scanGroup 0 "00,012 cells (Tiny) axis0 boxed  scan" m (boxed @Tiny),
      mapGroup 0 "00,400 cells (Small20) axis0 boxed  map" m bumpGrid bumpVec (boxed @Small20),
      scanGroup 0 "00,400 cells (Small20) axis0 boxed  scan" m (boxed @Small20),
      mapGroup 0 "01,024 cells (Small32) axis0 boxed  map" m bumpGrid bumpVec (boxed @Small32),
      scanGroup 0 "01,024 cells (Small32) axis0 boxed  scan" m (boxed @Small32),
      mapGroup 0 "02,500 cells (Step) axis0 boxed  map" m bumpGrid bumpVec (boxed @Step),
      scanGroup 0 "02,500 cells (Step) axis0 boxed  scan" m (boxed @Step),
      mapGroup 0 "02,500 cells (Step) axis0 unboxed  map" m bumpGrid bumpVec (unboxed @Step),
      scanGroup 0 "02,500 cells (Step) axis0 unboxed  scan" m (unboxed @Step),
      mapGroup 1 "02,500 cells (Step) axis1 unboxed  map" m bumpGrid bumpVec (unboxed @Step),
      scanGroup 1 "02,500 cells (Step) axis1 unboxed  scan" m (unboxed @Step),
      mapGroup 0 "10,000 cells (Mid) axis0 boxed  map" m bumpGrid bumpVec (boxed @Mid),
      scanGroup 0 "10,000 cells (Mid) axis0 boxed  scan" m (boxed @Mid),
      mapGroup 0 "10,000 cells (Mid) axis0 unboxed  map" m bumpGrid bumpVec (unboxed @Mid),
      scanGroup 0 "10,000 cells (Mid) axis0 unboxed  scan" m (unboxed @Mid),
      mapGroup 1 "10,000 cells (Mid) axis1 unboxed  map" m bumpGrid bumpVec (unboxed @Mid),
      scanGroup 1 "10,000 cells (Mid) axis1 unboxed  scan" m (unboxed @Mid),
      mapGroup 0 "40,000 cells (Wide) axis0 unboxed  map" m bumpGrid bumpVec (unboxed @Wide),
      scanGroup 0 "40,000 cells (Wide) axis0 unboxed  scan" m (unboxed @Wide),
      -- The working size, both axes and both representations.
      mapGroup 0 "90,000 cells (Big) axis0 boxed  map" m bumpGrid bumpVec (boxed @Big),
      scanGroup 0 "90,000 cells (Big) axis0 boxed  scan" m (boxed @Big),
      mapGroup 0 "90,000 cells (Big) axis0 unboxed  map" m bumpGrid bumpVec (unboxed @Big),
      scanGroup 0 "90,000 cells (Big) axis0 unboxed  scan" m (unboxed @Big),
      mapGroup 1 "90,000 cells (Big) axis1 boxed  map" m bumpGrid bumpVec (boxed @Big),
      scanGroup 1 "90,000 cells (Big) axis1 boxed  scan" m (boxed @Big),
      mapGroup 1 "90,000 cells (Big) axis1 unboxed  map" m bumpGrid bumpVec (unboxed @Big),
      scanGroup 1 "90,000 cells (Big) axis1 unboxed  scan" m (unboxed @Big),
      -- The same 90,000 cells with the fibres counted differently.
      mapGroup 0 "90,000 cells (Thin 4x22500) axis0 unboxed  map" m bumpGrid bumpVec (unboxed @Thin),
      scanGroup 0 "90,000 cells (Thin 4x22500) axis0 unboxed  scan" m (unboxed @Thin),
      mapGroup 1 "90,000 cells (Thin 4x22500) axis1 unboxed  map" m bumpGrid bumpVec (unboxed @Thin),
      scanGroup 1 "90,000 cells (Thin 4x22500) axis1 unboxed  scan" m (unboxed @Thin),
      -- A middle axis of a 3-D grid: stride 100, block 3,000, 30 blocks.
      mapGroup 1 "90,000 cells (Cube 30x30x100) axis1 unboxed  map" m bumpGrid bumpVec (unboxed @Cube),
      scanGroup 1 "90,000 cells (Cube 30x30x100) axis1 unboxed  scan" m (unboxed @Cube),
      -- A fibre function that costs something, so the cheap-workload result
      -- is not mistaken for a statement about 'mapAxis' in general.
      mapGroup 0 "90,000 cells (Big) axis0 unboxed  map scanl1" m scanlGrid scanlVec (unboxed @Big),
      mapGroup 1 "90,000 cells (Big) axis1 unboxed  map scanl1" m scanlGrid scanlVec (unboxed @Big),
      -- The only shape that misses cache; see 'Huge'.
      mapGroup 0 "4,000,000 cells (Huge) axis0 unboxed  map" m bumpGrid bumpVec (unboxed @Huge),
      scanGroup 0 "4,000,000 cells (Huge) axis0 unboxed  scan" m (unboxed @Huge),
      mapGroup 1 "4,000,000 cells (Huge) axis1 unboxed  map" m bumpGrid bumpVec (unboxed @Huge),
      scanGroup 1 "4,000,000 cells (Huge) axis1 unboxed  scan" m (unboxed @Huge),
      chunkGroup 0 "chunks per capability, 90,000 (Big) axis0 unboxed" (unboxed @Big),
      chunkGroup 1 "chunks per capability, 90,000 (Big) axis1 unboxed" (unboxed @Big)
    ]
