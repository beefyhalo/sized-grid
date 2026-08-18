{-# LANGUAGE DataKinds #-}

module Test.Klein
  ( kleinTests
  ) where

import           Data.Atlas.Topology.Seam (HalfEdge, seamViolations)
import           Data.Grid.Atlas       (AtlasCoord)
import           Data.Grid.Atlas.Klein
import           Data.Grid.Sized

import           Test.Tasty
import           Test.Tasty.HUnit

allAxes :: [Axis]
allAxes = [minBound .. maxBound]

allExtrema :: [Extremum]
allExtrema = [minBound .. maxBound]

kleinHalfEdges :: [HalfEdge () (Axis, Extremum)]
kleinHalfEdges = [((), (a, e)) | a <- allAxes, e <- allExtrema]

kleinSeamPairsUp :: TestTree
kleinSeamPairsUp =
    testCase "kleinSeam pairs every half-edge with one that points back" $
    assertEqual
        "half-edges whose partner does not point back"
        []
        (seamViolations kleinSeam kleinHalfEdges)

type W = 5

type H = 3

w :: Int
w = ordinalSize @W

h :: Int
h = ordinalSize @H

at :: Int -> Int -> AtlasCoord '[ Clamped W, Clamped H] 1
at u v =
    ( minBound
    , Clamped (unsafeOrdinal u) :| Clamped (unsafeOrdinal v) :| EmptyCoord)

allCells :: [(Int, Int)]
allCells = [(u, v) | u <- [0 .. w - 1], v <- [0 .. h - 1]]

step ::
       (AtlasCoord '[ Clamped W, Clamped H] 1, Heading)
    -> (AtlasCoord '[ Clamped W, Clamped H] 1, Heading)
step = uncurry kleinStep

lap :: Int -> Heading -> (Int, Int) -> (AtlasCoord '[ Clamped W, Clamped H] 1, Heading)
lap n heading (u, v) = iterate step (at u v, heading) !! n

stepWith ::
       Heading
    -> AtlasCoord '[ Clamped W, Clamped H] 1
    -> AtlasCoord '[ Clamped W, Clamped H] 1
stepWith heading c = fst (kleinStep c heading)

kleinTwistedLapMirrors :: TestTree
kleinTwistedLapMirrors =
    testCase
        "kleinStep along Twisted mirrors the row after one lap and closes after two" $
    mapM_
        (\(u, v) ->
             let heading = Heading Twisted AtMax
             in do assertEqual
                       (show (u, v) ++ ": after one lap")
                       (at u (h - 1 - v), heading)
                       (lap w heading (u, v))
                   assertEqual
                       (show (u, v) ++ ": after two laps")
                       (at u v, heading)
                       (lap (2 * w) heading (u, v)))
        allCells

kleinRolledLapCloses :: TestTree
kleinRolledLapCloses =
    testCase "kleinStep along Rolled closes after one lap, unmirrored" $
    mapM_
        (\(u, v) ->
             let heading = Heading Rolled AtMax
             in assertEqual
                    (show (u, v) ++ ": after one lap")
                    (at u v, heading)
                    (lap h heading (u, v)))
        allCells

-- | The Klein bottle's fundamental group is non-abelian, and this is where a
-- walker can see it: the twist reverses the sense of the axis it crosses,
-- so a 'Rolled' step taken before the crossing is not the same one taken
-- after it. The projective plane is what a table with /both/ entries
-- reversed would describe; this test is what tells the two apart.
kleinTwistDoesNotCommute :: TestTree
kleinTwistDoesNotCommute =
    testCase "kleinStep: crossing Twisted does not commute with a Rolled step" $
    mapM_
        (\v ->
             let crossThenStep =
                     stepWith (Heading Rolled AtMax) $
                     stepWith (Heading Twisted AtMax) (at (w - 1) v)
                 stepThenCross =
                     stepWith (Heading Twisted AtMax) $
                     stepWith (Heading Rolled AtMax) (at (w - 1) v)
             in do assertEqual
                       (show v ++ ": cross then step")
                       (at 0 ((h - 1 - v + 1) `mod` h))
                       crossThenStep
                   assertEqual
                       (show v ++ ": step then cross")
                       (at 0 (h - 1 - ((v + 1) `mod` h)))
                       stepThenCross
                   assertBool
                       (show v ++ ": the two orders must differ")
                       (crossThenStep /= stepThenCross))
        [0 .. h - 1]

kleinTests :: TestTree
kleinTests =
    testGroup
        "Data.Grid.Atlas.Klein"
        [ kleinSeamPairsUp
        , kleinTwistedLapMirrors
        , kleinRolledLapCloses
        , kleinTwistDoesNotCommute
        ]
