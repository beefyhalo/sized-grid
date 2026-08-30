{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Which shape of loop an axis-removing fold should be, for
-- @sized-grid-7qbi@.
--
-- Five candidates, all with the same type, measured on the strided axis, the
-- contiguous axis, and the middle axis of a cube -- boxed and unboxed, at an
-- @Int@ accumulator and at a boxed-in-principle pair.
--
-- The candidates are checked before they are measured. 'verify' compares
-- every one of them against a reference written with coordinates -- shared
-- code with none of them -- on a 3x5 grid and a 2x3x4 cube, over a
-- non-associative and non-commutative operator, and exits non-zero on the
-- first disagreement. A candidate that is fast because it folds the wrong
-- cells fails rather than reports.
module Main (main) where

import Control.Monad (unless)
import Data.Grid.Sized
import Data.Grid.Sized.Unboxed (UGrid)
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import FoldAxis
import GHC.TypeLits (KnownNat, type (<=))
import System.Exit (exitFailure)
import Test.Tasty.Bench

-- * The shapes

--
-- 300x300 and 60x50x30 are both 90,000 cells, which is the size the library's
-- own @bench/Main.hs@ uses throughout, so a number here can be read next to
-- one in @bench/baseline-ghc9.12.3-aarch64-darwin.csv@.

type Big = '[Clamped 300, Clamped 300]

type Cube = '[Clamped 60, Clamped 50, Clamped 30]

-- | The verification shapes, taken from @tests/Test/Axis.hs@: distinct axis
-- sizes throughout, so a transposed or mis-strided result cannot be right by
-- accident, and a middle axis no composition of @transposeGrid@ reaches.
type Flat = '[Ordinal 3, Ordinal 5]

type Cube3 = '[Ordinal 2, Ordinal 3, Ordinal 4]

-- * The grids

bigGrid :: Grid Big Int
bigGrid = tabulateGrid coordPosition

ubigGrid :: UGrid Big Int
ubigGrid = tabulateGrid coordPosition

cubeGrid :: Grid Cube Int
cubeGrid = tabulateGrid coordPosition

ucubeGrid :: UGrid Cube Int
ucubeGrid = tabulateGrid coordPosition

-- * The references

--
-- Index by coordinate and let 'tabulateGrid' place the result, exactly as
-- @tests/Test/Axis.hs@ does: reproducing the size-and-stride arithmetic in
-- the reference would test nothing.

axisValues :: forall n. (KnownNat n, 1 <= n) => [Ordinal n]
axisValues = [minBound .. maxBound]

refFlat0 :: (Int -> Int -> Int) -> Int -> Grid Flat Int -> Grid '[Ordinal 5] Int
refFlat0 f z g =
  tabulateGrid $ \(j :| _) ->
    foldl' f z [indexGrid g (i :| j :| EmptyCoord) | i <- axisValues]

refFlat1 :: (Int -> Int -> Int) -> Int -> Grid Flat Int -> Grid '[Ordinal 3] Int
refFlat1 f z g =
  tabulateGrid $ \(i :| _) ->
    foldl' f z [indexGrid g (i :| j :| EmptyCoord) | j <- axisValues]

refCube0 ::
  (Int -> Int -> Int) -> Int -> Grid Cube3 Int -> Grid '[Ordinal 3, Ordinal 4] Int
refCube0 f z g =
  tabulateGrid $ \(j :| k :| _) ->
    foldl' f z [indexGrid g (i :| j :| k :| EmptyCoord) | i <- axisValues]

refCube1 ::
  (Int -> Int -> Int) -> Int -> Grid Cube3 Int -> Grid '[Ordinal 2, Ordinal 4] Int
refCube1 f z g =
  tabulateGrid $ \(i :| k :| _) ->
    foldl' f z [indexGrid g (i :| j :| k :| EmptyCoord) | j <- axisValues]

refCube2 ::
  (Int -> Int -> Int) -> Int -> Grid Cube3 Int -> Grid '[Ordinal 2, Ordinal 3] Int
refCube2 f z g =
  tabulateGrid $ \(i :| j :| _) ->
    foldl' f z [indexGrid g (i :| j :| k :| EmptyCoord) | k <- axisValues]

-- | Neither associative nor commutative, and the accumulator is the left
-- argument. @(+)@ cannot tell a fold that runs the wrong way along the axis
-- from one that does not, and cannot tell a wrong grouping either.
op :: Int -> Int -> Int
op acc x = 2 * acc - x

seed :: Int
seed = 7

-- * Verification

-- | Every candidate against the reference, boxed and unboxed, on every axis
-- of both verification shapes. Returns the failures.
failures :: [String]
failures =
  concat
    [ check "Flat/0 generate" (foldAxisGenerate 0 op seed flat) (refFlat0 op seed flat),
      check "Flat/0 write" (foldAxisWrite 0 op seed flat) (refFlat0 op seed flat),
      check "Flat/0 total" (foldAxisTotal 0 op seed flat) (refFlat0 op seed flat),
      check "Flat/0 nonempty" (foldAxisNonEmpty 0 op seed flat) (refFlat0 op seed flat),
      check "Flat/0 sweep" (foldAxisSweep 0 op seed flat) (refFlat0 op seed flat),
      check "Flat/0 fibres" (foldAxisFibres 0 op seed flat) (refFlat0 op seed flat),
      check "Flat/0 slices" (foldAxisSlices 0 op seed flat) (refFlat0 op seed flat),
      check "Flat/0 split" (foldAxisSplit op seed flat) (refFlat0 op seed flat),
      check "Flat/1 generate" (foldAxisGenerate 1 op seed flat) (refFlat1 op seed flat),
      check "Flat/1 total" (foldAxisTotal 1 op seed flat) (refFlat1 op seed flat),
      check "Flat/1 sweep" (foldAxisSweep 1 op seed flat) (refFlat1 op seed flat),
      check "Flat/1 fibres" (foldAxisFibres 1 op seed flat) (refFlat1 op seed flat),
      check "Flat/1 slices" (foldAxisSlices 1 op seed flat) (refFlat1 op seed flat),
      check "Cube3/0 generate" (foldAxisGenerate 0 op seed cube) (refCube0 op seed cube),
      check "Cube3/0 total" (foldAxisTotal 0 op seed cube) (refCube0 op seed cube),
      check "Cube3/0 sweep" (foldAxisSweep 0 op seed cube) (refCube0 op seed cube),
      check "Cube3/0 fibres" (foldAxisFibres 0 op seed cube) (refCube0 op seed cube),
      check "Cube3/0 slices" (foldAxisSlices 0 op seed cube) (refCube0 op seed cube),
      check "Cube3/0 split" (foldAxisSplit op seed cube) (refCube0 op seed cube),
      check "Cube3/1 generate" (foldAxisGenerate 1 op seed cube) (refCube1 op seed cube),
      check "Cube3/1 write" (foldAxisWrite 1 op seed cube) (refCube1 op seed cube),
      check "Cube3/1 total" (foldAxisTotal 1 op seed cube) (refCube1 op seed cube),
      check "Cube3/1 nonempty" (foldAxisNonEmpty 1 op seed cube) (refCube1 op seed cube),
      check "Cube3/1 sweep" (foldAxisSweep 1 op seed cube) (refCube1 op seed cube),
      check "Cube3/1 fibres" (foldAxisFibres 1 op seed cube) (refCube1 op seed cube),
      check "Cube3/1 slices" (foldAxisSlices 1 op seed cube) (refCube1 op seed cube),
      check "Cube3/2 generate" (foldAxisGenerate 2 op seed cube) (refCube2 op seed cube),
      check "Cube3/2 write" (foldAxisWrite 2 op seed cube) (refCube2 op seed cube),
      check "Cube3/2 total" (foldAxisTotal 2 op seed cube) (refCube2 op seed cube),
      check "Cube3/2 nonempty" (foldAxisNonEmpty 2 op seed cube) (refCube2 op seed cube),
      check "Cube3/2 sweep" (foldAxisSweep 2 op seed cube) (refCube2 op seed cube),
      check "Cube3/2 fibres" (foldAxisFibres 2 op seed cube) (refCube2 op seed cube),
      check "Cube3/2 slices" (foldAxisSlices 2 op seed cube) (refCube2 op seed cube),
      -- The unboxed representation runs the same code through a different
      -- 'VG.Vector' dictionary; a mutable-accumulator candidate is where the
      -- two could plausibly diverge, so both are checked rather than assumed.
      checkU "Flat/0 generate unboxed" (foldAxisGenerate 0 op seed uflat) (refFlat0 op seed flat),
      checkU "Flat/0 write unboxed" (foldAxisWrite 0 op seed uflat) (refFlat0 op seed flat),
      checkU "Flat/0 sweep unboxed" (foldAxisSweep 0 op seed uflat) (refFlat0 op seed flat),
      checkU "Cube3/1 generate unboxed" (foldAxisGenerate 1 op seed ucube) (refCube1 op seed cube),
      checkU "Cube3/1 sweep unboxed" (foldAxisSweep 1 op seed ucube) (refCube1 op seed cube),
      checkU "Cube3/2 slices unboxed" (foldAxisSlices 2 op seed ucube) (refCube2 op seed cube),
      -- The empty axis. 'IsCoordLifted' demands @1 <= CoordNat x@ but
      -- 'AllSizedKnown' does not, so a grid with a zero-sized axis is
      -- constructible through 'gridFromVector' without ever meeting that
      -- constraint -- and folding that axis away should give the seed at every
      -- remaining cell, three of them here. Every candidate but 'foldAxisTotal'
      -- divides by the axis's size to find that out, and so dies instead.
      check "empty axis, total" (foldr (:) [] (foldAxisTotal 1 op seed emptyGrid)) [seed, seed, seed]
    ]
  where
    flat = tabulateGrid coordPosition :: Grid Flat Int
    cube = tabulateGrid coordPosition :: Grid Cube3 Int
    uflat = tabulateGrid coordPosition :: UGrid Flat Int
    ucube = tabulateGrid coordPosition :: UGrid Cube3 Int
    emptyGrid = fromJust (gridFromVector V.empty) :: Grid '[Ordinal 3, Ordinal 0] Int

    check :: (Eq a, Show a) => String -> a -> a -> [String]
    check name got want
      | got == want = []
      | otherwise = [name ++ ": got " ++ show got ++ ", want " ++ show want]

    checkU ::
      (U.Unbox a, Eq a, Show a) => String -> UGrid cs a -> Grid cs a -> [String]
    checkU name got want = check name (U.toList (gridVector got)) (foldr (:) [] want)

-- * The benchmarks

main :: IO ()
main = do
  unless (null failures) $ do
    mapM_ putStrLn failures
    exitFailure
  defaultMain
    [ bgroup
        "axis 0 (strided, 300 fibres of 300) 300x300"
        [ env (pure bigGrid) $ \g ->
            bench "generate  boxed" $ nf (foldAxisGenerate 0 (+) 0) g,
          env (pure ubigGrid) $ \g ->
            bench "generate  unboxed" $ nf (foldAxisGenerate 0 (+) 0) g,
          env (pure bigGrid) $ \g ->
            bench "write     boxed" $ nf (foldAxisWrite 0 (+) 0) g,
          env (pure ubigGrid) $ \g ->
            bench "write     unboxed" $ nf (foldAxisWrite 0 (+) 0) g,
          env (pure bigGrid) $ \g ->
            bench "total     boxed" $ nf (foldAxisTotal 0 (+) 0) g,
          env (pure ubigGrid) $ \g ->
            bench "total     unboxed" $ nf (foldAxisTotal 0 (+) 0) g,
          env (pure bigGrid) $ \g ->
            bench "nonempty  boxed" $ nf (foldAxisNonEmpty 0 (+) 0) g,
          env (pure ubigGrid) $ \g ->
            bench "nonempty  unboxed" $ nf (foldAxisNonEmpty 0 (+) 0) g,
          env (pure bigGrid) $ \g ->
            bench "sweep     boxed" $ nf (foldAxisSweep 0 (+) 0) g,
          env (pure ubigGrid) $ \g ->
            bench "sweep     unboxed" $ nf (foldAxisSweep 0 (+) 0) g,
          env (pure bigGrid) $ \g ->
            bench "fibres    boxed" $ nf (foldAxisFibres 0 (+) 0) g,
          env (pure ubigGrid) $ \g ->
            bench "fibres    unboxed" $ nf (foldAxisFibres 0 (+) 0) g,
          env (pure bigGrid) $ \g ->
            bench "split     boxed" $ nf (foldAxisSplit (+) 0) g,
          env (pure ubigGrid) $ \g ->
            bench "split     unboxed" $ nf (foldAxisSplit (+) 0) g
        ],
      bgroup
        "axis 1 (contiguous, 300 fibres of 300) 300x300"
        [ env (pure bigGrid) $ \g ->
            bench "generate  boxed" $ nf (foldAxisGenerate 1 (+) 0) g,
          env (pure ubigGrid) $ \g ->
            bench "generate  unboxed" $ nf (foldAxisGenerate 1 (+) 0) g,
          env (pure bigGrid) $ \g ->
            bench "write     boxed" $ nf (foldAxisWrite 1 (+) 0) g,
          env (pure ubigGrid) $ \g ->
            bench "write     unboxed" $ nf (foldAxisWrite 1 (+) 0) g,
          env (pure bigGrid) $ \g ->
            bench "total     boxed" $ nf (foldAxisTotal 1 (+) 0) g,
          env (pure ubigGrid) $ \g ->
            bench "total     unboxed" $ nf (foldAxisTotal 1 (+) 0) g,
          env (pure bigGrid) $ \g ->
            bench "nonempty  boxed" $ nf (foldAxisNonEmpty 1 (+) 0) g,
          env (pure ubigGrid) $ \g ->
            bench "nonempty  unboxed" $ nf (foldAxisNonEmpty 1 (+) 0) g,
          env (pure bigGrid) $ \g ->
            bench "sweep     boxed" $ nf (foldAxisSweep 1 (+) 0) g,
          env (pure ubigGrid) $ \g ->
            bench "sweep     unboxed" $ nf (foldAxisSweep 1 (+) 0) g,
          env (pure bigGrid) $ \g ->
            bench "fibres    boxed" $ nf (foldAxisFibres 1 (+) 0) g,
          env (pure ubigGrid) $ \g ->
            bench "fibres    unboxed" $ nf (foldAxisFibres 1 (+) 0) g,
          env (pure bigGrid) $ \g ->
            bench "slices    boxed" $ nf (foldAxisSlices 1 (+) 0) g,
          env (pure ubigGrid) $ \g ->
            bench "slices    unboxed" $ nf (foldAxisSlices 1 (+) 0) g
        ],
      bgroup
        "axis 1 of a cube (60x50x30), the middle axis"
        [ env (pure cubeGrid) $ \g ->
            bench "generate  boxed" $ nf (foldAxisGenerate 1 (+) 0) g,
          env (pure ucubeGrid) $ \g ->
            bench "generate  unboxed" $ nf (foldAxisGenerate 1 (+) 0) g,
          env (pure cubeGrid) $ \g ->
            bench "write     boxed" $ nf (foldAxisWrite 1 (+) 0) g,
          env (pure ucubeGrid) $ \g ->
            bench "write     unboxed" $ nf (foldAxisWrite 1 (+) 0) g,
          env (pure cubeGrid) $ \g ->
            bench "total     boxed" $ nf (foldAxisTotal 1 (+) 0) g,
          env (pure ucubeGrid) $ \g ->
            bench "total     unboxed" $ nf (foldAxisTotal 1 (+) 0) g,
          env (pure cubeGrid) $ \g ->
            bench "nonempty  boxed" $ nf (foldAxisNonEmpty 1 (+) 0) g,
          env (pure ucubeGrid) $ \g ->
            bench "nonempty  unboxed" $ nf (foldAxisNonEmpty 1 (+) 0) g,
          env (pure cubeGrid) $ \g ->
            bench "sweep     boxed" $ nf (foldAxisSweep 1 (+) 0) g,
          env (pure ucubeGrid) $ \g ->
            bench "sweep     unboxed" $ nf (foldAxisSweep 1 (+) 0) g,
          env (pure cubeGrid) $ \g ->
            bench "fibres    boxed" $ nf (foldAxisFibres 1 (+) 0) g,
          env (pure ucubeGrid) $ \g ->
            bench "fibres    unboxed" $ nf (foldAxisFibres 1 (+) 0) g
        ],
      bgroup
        "type-changing fold, Int -> (Int, Int), axis 0 300x300"
        [ env (pure bigGrid) $ \g ->
            bench "generate  boxed" $ nf (foldAxisGenerate 0 minMax (maxBound, minBound)) g,
          env (pure ubigGrid) $ \g ->
            bench "generate  unboxed" $ nf (foldAxisGenerate 0 minMax (maxBound, minBound)) g,
          env (pure bigGrid) $ \g ->
            bench "write     boxed" $ nf (foldAxisWrite 0 minMax (maxBound, minBound)) g,
          env (pure ubigGrid) $ \g ->
            bench "write     unboxed" $ nf (foldAxisWrite 0 minMax (maxBound, minBound)) g,
          env (pure bigGrid) $ \g ->
            bench "nonempty  boxed" $ nf (foldAxisNonEmpty 0 minMax (maxBound, minBound)) g,
          env (pure ubigGrid) $ \g ->
            bench "nonempty  unboxed" $ nf (foldAxisNonEmpty 0 minMax (maxBound, minBound)) g,
          env (pure bigGrid) $ \g ->
            bench "sweep     boxed" $ nf (foldAxisSweep 0 minMax (maxBound, minBound)) g,
          env (pure ubigGrid) $ \g ->
            bench "sweep     unboxed" $ nf (foldAxisSweep 0 minMax (maxBound, minBound)) g
        ],
      -- Read these two next to everything above: the whole-grid fold visits
      -- the same 90,000 cells and writes one answer instead of 300.
      bgroup
        "for scale: the whole-grid fold the library already has"
        [ bench "foldlGrid' 300x300   boxed" $ whnf (foldlGrid' (+) 0) bigGrid,
          bench "foldlGrid' 300x300 unboxed" $ whnf (foldlGrid' (+) 0) ubigGrid
        ]
    ]
  where
    minMax :: (Int, Int) -> Int -> (Int, Int)
    minMax (!lo, !hi) x = (min lo x, max hi x)
