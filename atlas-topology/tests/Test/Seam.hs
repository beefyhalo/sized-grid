-- | Tests for "Data.Atlas.Topology.Seam".
module Test.Seam
  ( seamTests
  ) where

import           Data.Atlas.Topology.Seam

import           Data.List             (sort)
import           Test.QuickCheck       (Gen, elements, forAll, shuffle,
                                        vectorOf, (===))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

-- | Three charts and four boundary labels each: 12 half-edges, an even
-- number, so a perfect matching exists to generate against.
data Chart
    = A
    | B
    | C
    deriving (Eq, Ord, Show, Enum, Bounded)

data Side
    = N
    | E
    | S
    | W
    deriving (Eq, Ord, Show, Enum, Bounded)

allHalfEdges :: [HalfEdge Chart Side]
allHalfEdges = [(c, s) | c <- [minBound .. maxBound], s <- [minBound .. maxBound]]

-- | A seam table as an association list, so a test can inspect and perturb
-- it before handing it to 'SeamTable'.
type Gluing = [(HalfEdge Chart Side, (HalfEdge Chart Side, Bool))]

tableOf :: Gluing -> SeamTable Chart Side
tableOf g =
    SeamTable $ \c s ->
        case lookup (c, s) g of
            Just ((c', s'), flipped) -> (c', s', flipped)
            Nothing -> error ("tableOf: incomplete gluing at " ++ show (c, s))

-- | Pair up all 12 half-edges at random, give each pair its own orientation
-- bit, and record both directions of every pair.
genGluing :: Gen Gluing
genGluing = do
    shuffled <- shuffle allHalfEdges
    let pairs = twos shuffled
    flips <- vectorOf (length pairs) (elements [False, True])
    pure
        [ entry
        | ((h1, h2), flipped) <- zip pairs flips
        , entry <- [(h1, (h2, flipped)), (h2, (h1, flipped))]
        ]
  where
    twos (x:y:rest) = (x, y) : twos rest
    twos _          = []

matchingsAreInvolutions :: TestTree
matchingsAreInvolutions =
    testProperty "every perfect matching of half-edges is a valid seam table" $
    forAll genGluing $ \g -> seamViolations (tableOf g) allHalfEdges === []

-- | Negate the flip bit of one half-edge only, leaving its partner saying
-- the opposite: the two now disagree about whether crossing the seam
-- reverses the along-seam direction.
oneBrokenSeamIsFoundExactly :: TestTree
oneBrokenSeamIsFoundExactly =
    testProperty "negating one half-edge's flip bit breaks exactly that seam" $
    forAll ((,) <$> genGluing <*> elements allHalfEdges) $ \(g, broken) ->
        let mutate (h, (dest, flipped))
                | h == broken = (h, (dest, not flipped))
                | otherwise = (h, (dest, flipped))
            partner = maybe broken fst (lookup broken g)
        in sort (seamViolations (tableOf (map mutate g)) allHalfEdges) ===
           sort [broken, partner]

-- | One chart, its two ends glued to each other the same way round: a
-- cylinder.
cylinder :: SeamTable () Side
cylinder =
    SeamTable $ \_ s ->
        case s of
            W -> ((), E, False)
            E -> ((), W, False)
            other -> ((), other, False)

-- | The same two ends glued with a twist --- a Mobius band.
mobius :: SeamTable () Side
mobius =
    SeamTable $ \_ s ->
        case s of
            W -> ((), E, True)
            E -> ((), W, True)
            other -> ((), other, False)

-- | One end twisted, the other not.
badMobius :: SeamTable () Side
badMobius =
    SeamTable $ \_ s ->
        case s of
            W -> ((), E, True)
            E -> ((), W, False)
            other -> ((), other, False)

byHandTables :: TestTree
byHandTables =
    testGroup
        "hand-written tables"
        [ testCase "a cylinder is a valid gluing" $
          assertEqual "" [] (seamViolations cylinder units)
        , testCase "a Mobius band is a valid gluing" $
          assertEqual "" [] (seamViolations mobius units)
        , testCase "sides disagreeing about the twist is not" $
          assertEqual "" [((), E), ((), W)] (seamViolations badMobius units)
        ]
  where
    units = [((), s) | s <- [minBound .. maxBound] :: [Side]]

seamTests :: TestTree
seamTests =
    testGroup
        "Data.Atlas.Topology.Seam"
        [matchingsAreInvolutions, oneBrokenSeamIsFoundExactly, byHandTables]
