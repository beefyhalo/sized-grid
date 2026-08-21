{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  ( axisTests
  ) where

import           Data.Grid.Sized

-- The orphan 'Arbitrary' instance for 'Grid'.
import           Test.Arbitrary        ()

import           Data.Maybe            (fromJust)
import           GHC.TypeLits          (KnownNat, type (<=))
import           Test.Tasty
import           Test.Tasty.QuickCheck

-- | Three axes, three sizes, and the middle one is reachable by no
-- composition of 'transposeGrid'.
type Cube = '[ Ordinal 2, Ordinal 3, Ordinal 4]

type Flat = '[ Ordinal 3, Ordinal 5]

-- | Every value of an axis, in index order.
axisValues :: forall n. (KnownNat n, 1 <= n) => [Ordinal n]
axisValues = [minBound .. maxBound]

-- | A one-axis grid from its elements. Total: every list passed to it is
-- built by 'axisValues' and so has exactly the axis's length.
fibre :: forall n. KnownNat n => [Int] -> Grid '[ Ordinal n] Int
fibre = fromJust . gridFromList

at :: forall n. (KnownNat n, 1 <= n) => Grid '[ Ordinal n] Int -> Ordinal n -> Int
at g i = indexGrid g (i :| EmptyCoord)

-- | A fibre transform that moves elements around rather than mapping them in
-- place, so a fibre gathered in the wrong order is not still correct by
-- accident the way @'mapGrid' (+ 1)@ would leave it.
reverseFibre :: forall n. KnownNat n => Grid '[ Ordinal n] Int -> Grid '[ Ordinal n] Int
reverseFibre = fibre . reverse . foldr (:) []

--------------------------------------------------------------------------------
-- The references: one per axis, indexing by coordinate.
--------------------------------------------------------------------------------

refCube0 ::
     (Grid '[ Ordinal 2] Int -> Grid '[ Ordinal 2] Int)
  -> Grid Cube Int
  -> Grid Cube Int
refCube0 f g =
  tabulateGrid $ \(i :| j :| k :| _) ->
    f (fibre [indexGrid g (i' :| j :| k :| EmptyCoord) | i' <- axisValues]) `at` i

refCube1 ::
     (Grid '[ Ordinal 3] Int -> Grid '[ Ordinal 3] Int)
  -> Grid Cube Int
  -> Grid Cube Int
refCube1 f g =
  tabulateGrid $ \(i :| j :| k :| _) ->
    f (fibre [indexGrid g (i :| j' :| k :| EmptyCoord) | j' <- axisValues]) `at` j

refCube2 ::
     (Grid '[ Ordinal 4] Int -> Grid '[ Ordinal 4] Int)
  -> Grid Cube Int
  -> Grid Cube Int
refCube2 f g =
  tabulateGrid $ \(i :| j :| k :| _) ->
    f (fibre [indexGrid g (i :| j :| k' :| EmptyCoord) | k' <- axisValues]) `at` k

refFlat0 ::
     (Grid '[ Ordinal 3] Int -> Grid '[ Ordinal 3] Int)
  -> Grid Flat Int
  -> Grid Flat Int
refFlat0 f g =
  tabulateGrid $ \(i :| j :| _) ->
    f (fibre [indexGrid g (i' :| j :| EmptyCoord) | i' <- axisValues]) `at` i

refFlat1 ::
     (Grid '[ Ordinal 5] Int -> Grid '[ Ordinal 5] Int)
  -> Grid Flat Int
  -> Grid Flat Int
refFlat1 f g =
  tabulateGrid $ \(i :| j :| _) ->
    f (fibre [indexGrid g (i :| j' :| EmptyCoord) | j' <- axisValues]) `at` j

--------------------------------------------------------------------------------

axisTests :: TestTree
axisTests =
  testGroup
    "mapAxis and scanAxis against a coordinate-indexed reference"
    [ testGroup
        "mapAxis agrees with the reference, on every axis"
        [ testProperty "2D, axis 0 (strided)" $ \(g :: Grid Flat Int) ->
            conjoin
              [ mapAxis 0 f g === refFlat0 f g | f <- flatFibre0 ]
        , testProperty "2D, axis 1 (contiguous)" $ \(g :: Grid Flat Int) ->
            conjoin
              [ mapAxis 1 f g === refFlat1 f g | f <- flatFibre1 ]
        , testProperty "3D, axis 0 (outermost)" $ \(g :: Grid Cube Int) ->
            conjoin [ mapAxis 0 f g === refCube0 f g | f <- cubeFibre0 ]
        , testProperty "3D, axis 1 (the middle axis no transpose reaches)" $ \(g :: Grid Cube Int) ->
            conjoin [ mapAxis 1 f g === refCube1 f g | f <- cubeFibre1 ]
        , testProperty "3D, axis 2 (innermost)" $ \(g :: Grid Cube Int) ->
            conjoin [ mapAxis 2 f g === refCube2 f g | f <- cubeFibre2 ]
        ]
      -- 'scanAxis' has its own body: it reads one element back rather than
      -- gathering a fibre, so it shares no code with 'mapAxis' beyond the
      -- size and stride. These check the equation the Haddock claims.
      --
      -- @(-)@ throughout, not @(+)@: a commutative operator cannot tell a
      -- scan that folds the wrong way round from one that does not, and the
      -- accumulator is the left argument (as in 'scanl1Grid', which is
      -- 'Data.Vector.Generic.scanl1''). @(-)@ is not associative either, so
      -- it also pins the order the elements are combined in.
    , testGroup
        "scanAxis is mapAxis (scanl1Grid f), by two separate implementations"
        [ testProperty "2D, axis 0" $ \(g :: Grid Flat Int) ->
            conjoin [scanAxis 0 op g === mapAxis 0 (scanl1Grid op) g | op <- ops]
        , testProperty "2D, axis 1" $ \(g :: Grid Flat Int) ->
            conjoin [scanAxis 1 op g === mapAxis 1 (scanl1Grid op) g | op <- ops]
        , testProperty "3D, axis 0" $ \(g :: Grid Cube Int) ->
            conjoin [scanAxis 0 op g === mapAxis 0 (scanl1Grid op) g | op <- ops]
        , testProperty "3D, axis 1" $ \(g :: Grid Cube Int) ->
            conjoin [scanAxis 1 op g === mapAxis 1 (scanl1Grid op) g | op <- ops]
        , testProperty "3D, axis 2" $ \(g :: Grid Cube Int) ->
            conjoin [scanAxis 2 op g === mapAxis 2 (scanl1Grid op) g | op <- ops]
          -- Not implied by the five above: they compare two implementations
          -- that agree about which cells make up a fibre, so a shared error
          -- about that cancels. This one names the answer.
        , testProperty "3D, axis 1, against the coordinate reference" $ \(g :: Grid Cube Int) ->
            conjoin
              [scanAxis 1 op g === refCube1 (scanl1Grid op) g | op <- ops]
        , testProperty "2D, axis 0, against the coordinate reference" $ \(g :: Grid Flat Int) ->
            conjoin
              [scanAxis 0 op g === refFlat0 (scanl1Grid op) g | op <- ops]
        ]
    , testGroup
        "the identities that hold for every axis"
        [ testProperty "mapAxis n id == id, 3D" $ \(g :: Grid Cube Int) ->
            conjoin
              [ mapAxis 0 id g === g
              , mapAxis 1 id g === g
              , mapAxis 2 id g === g
              ]
          -- Reversing twice is the identity fibre by fibre, so it is also the
          -- identity on the grid -- unless the gather and the scatter
          -- disagree about which fibre is which.
        , testProperty "reversing each fibre twice is the identity, 3D" $ \(g :: Grid Cube Int) ->
            conjoin
              [ mapAxis 0 reverseFibre (mapAxis 0 reverseFibre g) === g
              , mapAxis 1 reverseFibre (mapAxis 1 reverseFibre g) === g
              , mapAxis 2 reverseFibre (mapAxis 2 reverseFibre g) === g
              ]
        , testProperty "acting on one axis leaves the row sums of the others alone, 3D" $ \(g :: Grid Cube Int) ->
            sum (mapAxis 1 reverseFibre g) === sum g
        ]
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
