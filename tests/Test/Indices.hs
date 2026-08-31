-- | Tests for 'coordIndices' and 'coordIndices2': asking a coordinate where it
-- is, one index per axis.
--
-- The reference these are checked against is the axis list itself --- match
-- with @(':|')@, take each axis value's 'toAxisIndex' --- which is what every
-- consumer of this library wrote by hand before sized-grid-bzzy, and which
-- shares no code with the stride arithmetic under test.
--
-- The shapes below use distinct axis sizes and distinct boundary policies on
-- purpose. On a square grid a transposed answer is still the right shape, and
-- on a coordinate whose axes have the same policy there is no way to see that
-- the policy is read per axis.
module Test.Indices
  ( indicesTests,
  )
where

import Data.AffineSpace ((.-.))
import Data.Grid.Sized
import Data.Maybe (fromJust)
import GHC.TypeLits (KnownNat)
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (NonNegative (..), testProperty, (===))

hwOf :: (KnownNat n) => Int -> Clamped n
hwOf = Clamped . fromJust . numToOrdinal

peOf :: (KnownNat n) => Int -> Periodic n
peOf = Periodic . fromJust . numToOrdinal

-- | One bounded axis and one torus axis, at different sizes: the policy is
-- read per axis, and so is the size.
type Mixed = '[Clamped 3, Periodic 5]

mixc :: Int -> Int -> Coord Mixed
mixc a b = hwOf a :| peOf b :| EmptyCoord

type Cube = '[Ordinal 2, Clamped 3, Reflective 4]

cube :: Int -> Int -> Int -> Coord Cube
cube a b c =
  unsafeOrdinal a
    :| hwOf b
    :| Reflective (fromJust (numToOrdinal c))
    :| EmptyCoord

-- | The reference: the indices read off the axis values, one @(':|')@ match at
-- a time. What 'Sudoku.Board.coordRowCol' and 'Sokoban.Board.spotXY' were.
mixedByHand :: Coord Mixed -> [Int]
mixedByHand (a :| b :| EmptyCoord) = [toAxisIndex a, toAxisIndex b]

cubeByHand :: Coord Cube -> [Int]
cubeByHand (a :| b :| c :| EmptyCoord) =
  [toAxisIndex a, toAxisIndex b, toAxisIndex c]

coordIndicesTests :: TestTree
coordIndicesTests =
  testGroup
    "coordIndices reports every axis's own index, first axis first"
    [ testCase "the origin is zero on every axis" $
        assertEqual "" [0, 0] (coordIndices (mixc 0 0)),
      testCase "the far corner is the last index of every axis" $
        assertEqual "" [2, 4] (coordIndices (mixc 2 4)),
      testCase "the two axes are not interchangeable" $ do
        assertEqual "1,2" [1, 2] (coordIndices (mixc 1 2))
        assertEqual "2,1" [2, 1] (coordIndices (mixc 2 1)),
      testCase "three axes come out in order too" $
        assertEqual "" [1, 2, 3] (coordIndices (cube 1 2 3)),
      testCase "a coord with no axes has no indices to report" $
        assertEqual "" [] (coordIndices EmptyCoord),
      testProperty "agrees with the axis values, two axes" $ \(c :: Coord Mixed) ->
        coordIndices c === mixedByHand c,
      testProperty "agrees with the axis values, three axes" $ \(c :: Coord Cube) ->
        coordIndices c === cubeByHand c,
      testProperty "one index per axis" $ \(c :: Coord Cube) ->
        length (coordIndices c) === axisCount @Cube
    ]

-- | The indices multiplied back out by the strides are the position they were
-- divided out of. This is the row-major layout stated the other way round, and
-- it is what makes 'coordIndices' usable for placing a cell: entry @k@ of
-- 'allCoord' has the indices that reconstruct @k@.
roundTripTests :: TestTree
roundTripTests =
  testGroup
    "the indices multiply back out to the position"
    [ testProperty "two axes" $ \(c :: Coord Mixed) ->
        rebuild [3, 5] (coordIndices c) === coordPosition c,
      testProperty "three axes" $ \(c :: Coord Cube) ->
        rebuild [2, 3, 4] (coordIndices c) === coordPosition c,
      testCase "over the whole space, in order" $
        assertEqual
          ""
          [0 .. coordSpaceSize @Mixed - 1]
          [rebuild [3, 5] (coordIndices c) | c <- allCoord @Mixed]
    ]
  where
    -- Row-major: the first axis is most significant, so each index is worth
    -- the product of the sizes to its right.
    rebuild :: [Int] -> [Int] -> Int
    rebuild sizes is = foldl' (\acc (n, i) -> acc * n + i) 0 (zip sizes is)

-- | 'coordFromIndices' is the inverse: one plain 'Int' per axis back into a
-- 'Coord', rejecting anything off the grid rather than folding it through a
-- boundary policy. The round-trip against 'coordIndices' is the property; the
-- rejections are the point of it for a parser.
coordFromIndicesTests :: TestTree
coordFromIndicesTests =
  testGroup
    "coordFromIndices is the inverse of coordIndices"
    [ testProperty "coordIndices then coordFromIndices round-trips, two axes" $
        \(c :: Coord Mixed) ->
          coordFromIndices (coordIndices c) === Just c,
      testProperty "coordIndices then coordFromIndices round-trips, three axes" $
        \(c :: Coord Cube) ->
          coordFromIndices (coordIndices c) === Just c,
      testCase "a concrete pair reaches the cell it names" $
        assertEqual "" (Just (mixc 2 4)) (coordFromIndices [2, 4]),
      testCase "no axes: the empty list is the empty coord" $
        assertEqual "" (Just EmptyCoord) (coordFromIndices @'[] []),
      testCase "a negative index is rejected, not clamped" $
        assertEqual "" Nothing (coordFromIndices @Mixed [-1, 0]),
      testCase "an index at the axis size is rejected, not wrapped" $
        assertEqual "" Nothing (coordFromIndices @Mixed [0, 5]),
      testCase "too few indices is a length mismatch" $
        assertEqual "" Nothing (coordFromIndices @Cube [1, 2]),
      testCase "too many indices is a length mismatch" $
        assertEqual "" Nothing (coordFromIndices @Mixed [1, 2, 3]),
      testProperty "any out-of-range axis rejects the whole list" $
        \(c :: Coord Cube) (NonNegative k) ->
          coordFromIndices @Cube (zipWith (+) (coordIndices c) [0, 0, k + 4])
            === Nothing
    ]

-- | Every index is a position on its own axis, so none of them is negative and
-- none reaches that axis's size. This is the property the two demos broke
-- before sized-grid-23y3 by asking @('.-.')@ instead: on a torus a
-- displacement from the origin is the shortest signed route, so the top half
-- of a periodic axis comes back negative.
inRangeTests :: TestTree
inRangeTests =
  testGroup
    "an index is a position and never a displacement"
    [ testProperty "every index is within its own axis" $ \(c :: Coord Mixed) ->
        and (zipWith (\n i -> 0 <= i && i < n) [3, 5] (coordIndices c)) === True,
      -- The concrete regression: on a 5-cell torus the last cell is 4, not
      -- -1, whatever the shorter route to it says.
      testCase "the last cell of a torus axis is its last index" $
        assertEqual "" [0, 4] (coordIndices (mixc 0 4)),
      testCase "and the displacement to it really is the other answer" $
        assertEqual
          ""
          (-1)
          ( case mixc 0 4 .-. zeroCoord of
              _ :^ d :^ NoDelta -> d
          )
    ]

coordIndices2Tests :: TestTree
coordIndices2Tests =
  testGroup
    "coordIndices2 is coordIndices at two axes, as a pair"
    [ testCase "first axis first" $
        assertEqual "" (1, 2) (coordIndices2 (mixc 1 2)),
      testCase "the far corner" $
        assertEqual "" (2, 4) (coordIndices2 (mixc 2 4)),
      testProperty "agrees with the list version" $ \(c :: Coord Mixed) ->
        [fst (coordIndices2 c), snd (coordIndices2 c)] === coordIndices c
    ]

indicesTests :: TestTree
indicesTests =
  testGroup
    "coordIndices"
    [ coordIndicesTests,
      roundTripTests,
      coordFromIndicesTests,
      inRangeTests,
      coordIndices2Tests
    ]
