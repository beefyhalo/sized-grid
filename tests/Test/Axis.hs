-- | Tests for 'mapAxis' and 'scanAxis', against references written with
-- coordinates.
--
-- Both are index arithmetic on a flat vector: an axis is a size and a stride,
-- and a fibre is @size@ elements @stride@ apart. A test that reproduces that
-- arithmetic tests nothing, so the references here index by 'Coord' instead
-- and let 'tabulateGrid' place the results -- the same operation stated in
-- terms that share no code with the thing under test.
--
-- The shapes have distinct axis sizes on purpose: on a square grid a
-- transposed or mis-strided result can still have the right shape, and on a
-- 2x2x2 the wrong axis can be indistinguishable from the right one.
module Test.Axis
  ( axisTests,
  )
where

-- The orphan 'Arbitrary' instance for 'Grid'.

import Control.Lens (itoListOf, toListOf)
import Data.Foldable (toList)
import Data.Grid.Sized
import Data.Maybe (fromJust)
import Data.Vector.Generic qualified as VG
import GHC.TypeLits (KnownNat, type (<=))
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- | Three axes, three sizes, and the middle one is reachable by no
-- composition of 'transposeGrid'.
type Cube = '[Ordinal 2, Ordinal 3, Ordinal 4]

type Flat = '[Ordinal 3, Ordinal 5]

-- | Every value of an axis, in index order.
axisValues :: forall n. (KnownNat n, 1 <= n) => [Ordinal n]
axisValues = [minBound .. maxBound]

-- | A one-axis grid from its elements. Total: every list passed to it is
-- built by 'axisValues' and so has exactly the axis's length.
fibre :: forall n. (KnownNat n) => [Int] -> Grid '[Ordinal n] Int
fibre = fromJust . gridFromList

at :: forall n. (KnownNat n, 1 <= n) => Grid '[Ordinal n] Int -> Ordinal n -> Int
at g i = indexGrid g (i :| EmptyCoord)

-- | A fibre transform that moves elements around rather than mapping them in
-- place, so a fibre gathered in the wrong order is not still correct by
-- accident the way @'mapGrid' (+ 1)@ would leave it.
reverseFibre :: forall n. (KnownNat n) => Grid '[Ordinal n] Int -> Grid '[Ordinal n] Int
reverseFibre = fibre . reverse . toList

--------------------------------------------------------------------------------
-- The references: one per axis, indexing by coordinate.
--------------------------------------------------------------------------------

refCube0 ::
  (Grid '[Ordinal 2] Int -> Grid '[Ordinal 2] Int) ->
  Grid Cube Int ->
  Grid Cube Int
refCube0 f g =
  tabulateGrid $ \(i :| j :| k :| _) ->
    f (fibre [indexGrid g (i' :| j :| k :| EmptyCoord) | i' <- axisValues]) `at` i

refCube1 ::
  (Grid '[Ordinal 3] Int -> Grid '[Ordinal 3] Int) ->
  Grid Cube Int ->
  Grid Cube Int
refCube1 f g =
  tabulateGrid $ \(i :| j :| k :| _) ->
    f (fibre [indexGrid g (i :| j' :| k :| EmptyCoord) | j' <- axisValues]) `at` j

refCube2 ::
  (Grid '[Ordinal 4] Int -> Grid '[Ordinal 4] Int) ->
  Grid Cube Int ->
  Grid Cube Int
refCube2 f g =
  tabulateGrid $ \(i :| j :| k :| _) ->
    f (fibre [indexGrid g (i :| j :| k' :| EmptyCoord) | k' <- axisValues]) `at` k

refFlat0 ::
  (Grid '[Ordinal 3] Int -> Grid '[Ordinal 3] Int) ->
  Grid Flat Int ->
  Grid Flat Int
refFlat0 f g =
  tabulateGrid $ \(i :| j :| _) ->
    f (fibre [indexGrid g (i' :| j :| EmptyCoord) | i' <- axisValues]) `at` i

refFlat1 ::
  (Grid '[Ordinal 5] Int -> Grid '[Ordinal 5] Int) ->
  Grid Flat Int ->
  Grid Flat Int
refFlat1 f g =
  tabulateGrid $ \(i :| j :| _) ->
    f (fibre [indexGrid g (i :| j' :| EmptyCoord) | j' <- axisValues]) `at` j

-- | References for foldAxis': fold each fibre and return a lower-dimensional grid
--
-- These fold along one axis and produce a grid with that axis removed.
refFoldFlat0 ::
  (Int -> Int -> Int) ->
  Int ->
  Grid Flat Int ->
  Grid '[Ordinal 5] Int
refFoldFlat0 f z g =
  tabulateGrid $ \(j :| _) ->
    foldl' f z [indexGrid g (i' :| j :| EmptyCoord) | i' <- axisValues]

refFoldFlat1 ::
  (Int -> Int -> Int) ->
  Int ->
  Grid Flat Int ->
  Grid '[Ordinal 3] Int
refFoldFlat1 f z g =
  tabulateGrid $ \(i :| _) ->
    foldl' f z [indexGrid g (i :| j' :| EmptyCoord) | j' <- axisValues]

refFoldCube0 ::
  (Int -> Int -> Int) ->
  Int ->
  Grid Cube Int ->
  Grid '[Ordinal 3, Ordinal 4] Int
refFoldCube0 f z g =
  tabulateGrid $ \(j :| k :| _) ->
    foldl' f z [indexGrid g (i' :| j :| k :| EmptyCoord) | i' <- axisValues]

refFoldCube1 ::
  (Int -> Int -> Int) ->
  Int ->
  Grid Cube Int ->
  Grid '[Ordinal 2, Ordinal 4] Int
refFoldCube1 f z g =
  tabulateGrid $ \(i :| k :| _) ->
    foldl' f z [indexGrid g (i :| j' :| k :| EmptyCoord) | j' <- axisValues]

refFoldCube2 ::
  (Int -> Int -> Int) ->
  Int ->
  Grid Cube Int ->
  Grid '[Ordinal 2, Ordinal 3] Int
refFoldCube2 f z g =
  tabulateGrid $ \(i :| j :| _) ->
    foldl' f z [indexGrid g (i :| j :| k' :| EmptyCoord) | k' <- axisValues]

refReduceFlat0 :: (Int -> Int -> Int) -> Grid Flat Int -> Grid '[Ordinal 5] Int
refReduceFlat0 f g =
  tabulateGrid $ \(j :| _) ->
    foldl1 f [indexGrid g (i' :| j :| EmptyCoord) | i' <- axisValues]

refReduceFlat1 :: (Int -> Int -> Int) -> Grid Flat Int -> Grid '[Ordinal 3] Int
refReduceFlat1 f g =
  tabulateGrid $ \(i :| _) ->
    foldl1 f [indexGrid g (i :| j' :| EmptyCoord) | j' <- axisValues]

refReduceCube0 ::
  (Int -> Int -> Int) -> Grid Cube Int -> Grid '[Ordinal 3, Ordinal 4] Int
refReduceCube0 f g =
  tabulateGrid $ \(j :| k :| _) ->
    foldl1 f [indexGrid g (i' :| j :| k :| EmptyCoord) | i' <- axisValues]

refReduceCube1 ::
  (Int -> Int -> Int) -> Grid Cube Int -> Grid '[Ordinal 2, Ordinal 4] Int
refReduceCube1 f g =
  tabulateGrid $ \(i :| k :| _) ->
    foldl1 f [indexGrid g (i :| j' :| k :| EmptyCoord) | j' <- axisValues]

refReduceCube2 ::
  (Int -> Int -> Int) -> Grid Cube Int -> Grid '[Ordinal 2, Ordinal 3] Int
refReduceCube2 f g =
  tabulateGrid $ \(i :| j :| _) ->
    foldl1 f [indexGrid g (i :| j :| k' :| EmptyCoord) | k' <- axisValues]

--------------------------------------------------------------------------------

axisTests :: TestTree
axisTests =
  testGroup
    "mapAxis and scanAxis against a coordinate-indexed reference"
    [ testGroup
        "mapAxis agrees with the reference, on every axis"
        [ testProperty "2D, axis 0 (strided)" $ \(g :: Grid Flat Int) ->
            conjoin
              [mapAxis 0 f g === refFlat0 f g | f <- flatFibre0],
          testProperty "2D, axis 1 (contiguous)" $ \(g :: Grid Flat Int) ->
            conjoin
              [mapAxis 1 f g === refFlat1 f g | f <- flatFibre1],
          testProperty "3D, axis 0 (outermost)" $ \(g :: Grid Cube Int) ->
            conjoin [mapAxis 0 f g === refCube0 f g | f <- cubeFibre0],
          testProperty "3D, axis 1 (the middle axis no transpose reaches)" $ \(g :: Grid Cube Int) ->
            conjoin [mapAxis 1 f g === refCube1 f g | f <- cubeFibre1],
          testProperty "3D, axis 2 (innermost)" $ \(g :: Grid Cube Int) ->
            conjoin [mapAxis 2 f g === refCube2 f g | f <- cubeFibre2]
        ],
      testCase "axisFold returns strided fibres in order" $
        let g = tabulateGrid coordPosition :: Grid Flat Int
         in map toList (toListOf (axisFold 0) g)
              @?= [ [0, 5, 10],
                    [1, 6, 11],
                    [2, 7, 12],
                    [3, 8, 13],
                    [4, 9, 14]
                  ],
      testCase "axisFold returns each fibre's ordinal offset" $
        let g = tabulateGrid coordPosition :: Grid Flat Int
         in map (fmap toList) (itoListOf (axisFold 0) g)
              @?= [ (unsafeOrdinal 0 :| EmptyCoord, [0, 5, 10]),
                    (unsafeOrdinal 1 :| EmptyCoord, [1, 6, 11]),
                    (unsafeOrdinal 2 :| EmptyCoord, [2, 7, 12]),
                    (unsafeOrdinal 3 :| EmptyCoord, [3, 8, 13]),
                    (unsafeOrdinal 4 :| EmptyCoord, [4, 9, 14])
                  ],
      -- 'scanAxis' has its own body: it reads one element back rather than
      -- gathering a fibre, so it shares no code with 'mapAxis' beyond the
      -- size and stride. These check the equation the Haddock claims.
      --
      -- @(-)@ throughout, not @(+)@: a commutative operator cannot tell a
      -- scan that folds the wrong way round from one that does not, and the
      -- accumulator is the left argument (as in 'scanl1Grid', which is
      -- 'Data.Vector.Generic.scanl1''). @(-)@ is not associative either, so
      -- it also pins the order the elements are combined in.
      testGroup
        "scanAxis is mapAxis (scanl1Grid f), by two separate implementations"
        [ testProperty "2D, axis 0" $ \(g :: Grid Flat Int) ->
            conjoin [scanAxis 0 op g === mapAxis 0 (scanl1Grid op) g | op <- ops],
          testProperty "2D, axis 1" $ \(g :: Grid Flat Int) ->
            conjoin [scanAxis 1 op g === mapAxis 1 (scanl1Grid op) g | op <- ops],
          testProperty "3D, axis 0" $ \(g :: Grid Cube Int) ->
            conjoin [scanAxis 0 op g === mapAxis 0 (scanl1Grid op) g | op <- ops],
          testProperty "3D, axis 1" $ \(g :: Grid Cube Int) ->
            conjoin [scanAxis 1 op g === mapAxis 1 (scanl1Grid op) g | op <- ops],
          testProperty "3D, axis 2" $ \(g :: Grid Cube Int) ->
            conjoin [scanAxis 2 op g === mapAxis 2 (scanl1Grid op) g | op <- ops],
          -- Not implied by the five above: they compare two implementations
          -- that agree about which cells make up a fibre, so a shared error
          -- about that cancels. This one names the answer.
          testProperty "3D, axis 1, against the coordinate reference" $ \(g :: Grid Cube Int) ->
            conjoin
              [scanAxis 1 op g === refCube1 (scanl1Grid op) g | op <- ops],
          testProperty "2D, axis 0, against the coordinate reference" $ \(g :: Grid Flat Int) ->
            conjoin
              [scanAxis 0 op g === refFlat0 (scanl1Grid op) g | op <- ops]
        ],
      testGroup
        "the identities that hold for every axis"
        [ testProperty "mapAxis n id == id, 3D" $ \(g :: Grid Cube Int) ->
            conjoin
              [ mapAxis 0 id g === g,
                mapAxis 1 id g === g,
                mapAxis 2 id g === g
              ],
          -- Reversing twice is the identity fibre by fibre, so it is also the
          -- identity on the grid -- unless the gather and the scatter
          -- disagree about which fibre is which.
          testProperty "reversing each fibre twice is the identity, 3D" $ \(g :: Grid Cube Int) ->
            conjoin
              [ mapAxis 0 reverseFibre (mapAxis 0 reverseFibre g) === g,
                mapAxis 1 reverseFibre (mapAxis 1 reverseFibre g) === g,
                mapAxis 2 reverseFibre (mapAxis 2 reverseFibre g) === g
              ],
          testProperty "acting on one axis leaves the row sums of the others alone, 3D" $ \(g :: Grid Cube Int) ->
            sum (mapAxis 1 reverseFibre g) === sum g
        ],
      testGroup
        "foldAxis' agrees with the reference, on every axis"
        [ testProperty "2D, axis 0 (strided)" $ \(g :: Grid Flat Int) ->
            conjoin
              [foldAxis' 0 f z g === refFoldFlat0 f z g | f <- foldOps, z <- seeds],
          testProperty "2D, axis 1 (contiguous)" $ \(g :: Grid Flat Int) ->
            conjoin
              [foldAxis' 1 f z g === refFoldFlat1 f z g | f <- foldOps, z <- seeds],
          testProperty "3D, axis 0 (outermost)" $ \(g :: Grid Cube Int) ->
            conjoin [foldAxis' 0 f z g === refFoldCube0 f z g | f <- foldOps, z <- seeds],
          testProperty "3D, axis 1 (the middle axis no transpose reaches)" $ \(g :: Grid Cube Int) ->
            conjoin [foldAxis' 1 f z g === refFoldCube1 f z g | f <- foldOps, z <- seeds],
          testProperty "3D, axis 2 (innermost)" $ \(g :: Grid Cube Int) ->
            conjoin [foldAxis' 2 f z g === refFoldCube2 f z g | f <- foldOps, z <- seeds]
        ],
      testGroup
        "foldAxis' order and grouping are pinned with non-associative operators"
        [ testProperty "2D, axis 0, order is left-to-right" $ \(g :: Grid Flat Int) ->
            conjoin [foldAxis' 0 f z g === refFoldFlat0 f z g | f <- nonAssocOps, z <- seeds],
          testProperty "2D, axis 1, order is left-to-right" $ \(g :: Grid Flat Int) ->
            conjoin [foldAxis' 1 f z g === refFoldFlat1 f z g | f <- nonAssocOps, z <- seeds],
          testProperty "3D, axis 1, order is left-to-right" $ \(g :: Grid Cube Int) ->
            conjoin [foldAxis' 1 f z g === refFoldCube1 f z g | f <- nonAssocOps, z <- seeds]
        ],
      testGroup
        "foldAxis' agrees with the published-API spelling"
        [ testProperty "2D, axis 0" $ \(g :: Grid Flat Int) ->
            foldAxis' 0 (+) 0 g
              === (fromJust . gridFromVector . VG.fromList . map (foldlGrid' (+) 0) . axisFibres 0) g,
          testProperty "2D, axis 1" $ \(g :: Grid Flat Int) ->
            foldAxis' 1 (+) 0 g
              === (fromJust . gridFromVector . VG.fromList . map (foldlGrid' (+) 0) . axisFibres 1) g,
          testProperty "3D, axis 0" $ \(g :: Grid Cube Int) ->
            foldAxis' 0 (+) 0 g
              === (fromJust . gridFromVector . VG.fromList . map (foldlGrid' (+) 0) . axisFibres 0) g,
          testProperty "3D, axis 1" $ \(g :: Grid Cube Int) ->
            foldAxis' 1 (+) 0 g
              === (fromJust . gridFromVector . VG.fromList . map (foldlGrid' (+) 0) . axisFibres 1) g,
          testProperty "3D, axis 2" $ \(g :: Grid Cube Int) ->
            foldAxis' 2 (+) 0 g
              === (fromJust . gridFromVector . VG.fromList . map (foldlGrid' (+) 0) . axisFibres 2) g
        ],
      testGroup
        "foldAxis' on one-axis grids"
        [ testCase "foldAxis' 0 reduces axis to a sum" $
            let g = fibre [1, 2, 3] :: Grid '[Ordinal 3] Int
                result = foldAxis' 0 (+) (0 :: Int) g
             in gridVector result @?= VG.fromList [6]
        ],
      testGroup
        "foldAxis' with non-associative operators shows order"
        [ testCase "foldAxis' 0 with (-) shows left-to-right evaluation" $
            let g = fibre [1, 2, 3] :: Grid '[Ordinal 3] Int
                result = foldAxis' 0 (-) 10 g
             in gridVector result @?= VG.fromList [4]
        ],
      testGroup
        "reduceAxis agrees with the seedless coordinate reference, on every axis"
        [ testProperty "2D, axis 0 (strided)" $ \(g :: Grid Flat Int) ->
            conjoin [reduceAxis 0 f g === refReduceFlat0 f g | f <- nonAssocOps],
          testProperty "2D, axis 1 (contiguous)" $ \(g :: Grid Flat Int) ->
            conjoin [reduceAxis 1 f g === refReduceFlat1 f g | f <- nonAssocOps],
          testProperty "3D, axis 0 (outermost)" $ \(g :: Grid Cube Int) ->
            conjoin [reduceAxis 0 f g === refReduceCube0 f g | f <- nonAssocOps],
          testProperty "3D, axis 1 (middle)" $ \(g :: Grid Cube Int) ->
            conjoin [reduceAxis 1 f g === refReduceCube1 f g | f <- nonAssocOps],
          testProperty "3D, axis 2 (innermost)" $ \(g :: Grid Cube Int) ->
            conjoin [reduceAxis 2 f g === refReduceCube2 f g | f <- nonAssocOps]
        ],
      testCase "reduceAxis seeds each fibre from its first element" $
        let g = fibre [1, 2, 3] :: Grid '[Ordinal 3] Int
         in gridVector (reduceAxis 0 (-) g) @?= VG.fromList [-4]
    ]
  where
    -- One transform that reorders the fibre, one that folds along it
    -- non-commutatively, one that only maps in place.
    flatFibre0 = [scanl1Grid (-), reverseFibre, mapGrid (* 2)]
    flatFibre1 = [scanl1Grid (-), reverseFibre, mapGrid (* 2)]
    cubeFibre0 = [scanl1Grid (-), reverseFibre, mapGrid (* 2)]
    cubeFibre1 = [scanl1Grid (-), reverseFibre, mapGrid (* 2)]
    cubeFibre2 = [scanl1Grid (-), reverseFibre, mapGrid (* 2)]
    ops = [(-), (+)]
    -- Non-associative and non-commutative operators for foldAxis' testing
    foldOps = [(+), (-), (*)]
    nonAssocOps = [(-), \acc x -> 2 * acc - x]
    seeds = [0, 1, 10]
