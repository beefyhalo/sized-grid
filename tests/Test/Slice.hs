{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Slice
  ( sliceTests
  ) where

import           Control.Lens          ((.~), (^.), over)
import           Data.Foldable         (toList)
import           Data.Grid.Sized
import           Data.Maybe            (fromJust)
import           Test.Arbitrary        ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (Property, testProperty, (===))

source :: Grid '[Ordinal 5] Int
source = fromJust $ gridFromList [1, 2, 3, 4, 5]

replacement :: Grid '[Ordinal 2] Int
replacement = fromJust $ gridFromList [20, 30]

sliceTests :: TestTree
sliceTests =
    testGroup
        "Slice lenses"
        [ testCase "slice reads the requested window" $
          assertEqual "slice" [2, 3] (toList (source ^. slice 1 2))
        , testCase "slice replaces only the requested window" $
          assertEqual "slice set" [1, 20, 30, 4, 5]
            (toList ((slice 1 2 .~ replacement) source))
        , testCase "prefix replaces only the prefix" $
          assertEqual "prefix set" [20, 30, 3, 4, 5]
            (toList ((prefix 2 .~ replacement) source))
        , testCase "suffix reads and replaces the suffix" $
          assertEqual "suffix set" [1, 2, 3, 20, 30]
            (toList ((suffix 3 .~ replacement) source))
        , testCase "slice identity and put-get laws" $ do
          assertEqual "identity" source (over (slice 1 2) id source)
          assertEqual "put-get" replacement ((slice 1 2 .~ replacement) source ^. slice 1 2)
        , policyTests
        ]

-- | A source with a real boundary policy, to pin sized-grid-pnws.
--
-- Every one of these was previously statable only at an 'Ordinal' source ---
-- not because the arithmetic differed, but because the result type named the
-- source's own axis, so writing @Grid \'[Periodic 3]@ on the left of the
-- @===@ was the only thing that typechecked, and that type was the bug. That
-- these compile /at all/ with 'Ordinal' on the right is half of what is being
-- checked here; the '===' checks the cells did not move while the types were
-- changing.
--
-- The corresponding \"and the wrong type is now rejected\" half is
-- @tests\/compile-fail\/RestrictionKeepsSourcePolicy.hs@, which no runtime
-- assertion can express.
periodicNine :: Grid '[ Periodic 9] Int
periodicNine = fromJust $ gridFromList [1 .. 9]

takeIsTake :: Grid '[ Periodic 9] Int -> Property
takeIsTake g = toList (takeGrid 3 g :: Grid '[ Ordinal 3] Int)
           === take 3 (toList g)

dropIsDrop :: Grid '[ Periodic 9] Int -> Property
dropIsDrop g = toList (dropGrid 3 g :: Grid '[ Ordinal 6] Int)
           === drop 3 (toList g)

-- | 'sliceGrid' itself is not exported --- 'slice' is its public face --- so
-- the same law is stated through the lens, at the other policy.
sliceIsTakeDrop :: Grid '[ Clamped 9] Int -> Property
sliceIsTakeDrop g = toList (g ^. slice 2 4 :: Grid '[ Ordinal 4] Int)
                === take 4 (drop 2 (toList g))

-- | The two halves partition the source and neither loses a cell on the way.
splitPartitions :: Grid '[ Periodic 9, Clamped 2] Int -> Property
splitPartitions g =
  let (a, b) = splitHigherDim g ::
        ( Grid '[ Ordinal 4, Clamped 2] Int
        , Grid '[ Ordinal 5, Clamped 2] Int )
   in toList a ++ toList b === toList g

-- | The lenses read the same window the bare functions do, at a source whose
-- axis has a policy the window must not inherit.
lensReadsWindow :: Grid '[ Periodic 9] Int -> Property
lensReadsWindow g =
  ( toList (g ^. slice 2 4 :: Grid '[ Ordinal 4] Int)
  , toList (g ^. prefix 3  :: Grid '[ Ordinal 3] Int)
  , toList (g ^. suffix 3  :: Grid '[ Ordinal 6] Int) )
  === ( take 4 (drop 2 (toList g))
      , take 3 (toList g)
      , drop 3 (toList g) )

-- | Writing an 'Ordinal' window back into a policy-carrying source replaces
-- exactly that window. The replacement's own axis is 'Ordinal' because the
-- splice never consults a policy --- it is @len@ cells and an offset.
lensWritesWindow :: Grid '[ Periodic 9] Int -> Grid '[ Ordinal 4] Int -> Property
lensWritesWindow g w =
  toList ((slice 2 4 .~ w) g)
    === take 2 (toList g) ++ toList w ++ drop 6 (toList g)

policyTests :: TestTree
policyTests =
    testGroup
        "a restriction destroys the boundary policy"
        [ testCase "takeGrid of a periodic source keeps the cells" $
          assertEqual "take"
            [1, 2, 3]
            (toList (takeGrid 3 periodicNine :: Grid '[ Ordinal 3] Int))
        , testCase "dropGrid of a periodic source keeps the cells" $
          assertEqual "drop"
            [4, 5, 6, 7, 8, 9]
            (toList (dropGrid 3 periodicNine :: Grid '[ Ordinal 6] Int))
        , testProperty "takeGrid 3 is take 3" takeIsTake
        , testProperty "dropGrid 3 is drop 3" dropIsDrop
        , testProperty "slice 2 4 is take 4 . drop 2, Clamped source" sliceIsTakeDrop
        , testProperty "splitHigherDim partitions the source" splitPartitions
        , testProperty "the slice lenses read the window" lensReadsWindow
        , testProperty "the slice lenses write the window" lensWritesWindow
        ]
