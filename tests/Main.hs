module Main
  ( main
  ) where

import           Data.Grid.Sized

import           Test.Arbitrary ()
import           Test.Axis
import           Test.Boundary
import           Test.CompileFail
import           Test.Focused
import           Test.Invariant
import           Test.Neighbours
import           Test.Ordinal
import           Test.Path
import           Test.Periodic
import           Test.Ray
import           Test.Reflective
import           Test.Shrink
import           Test.Slice
import           Test.Stencil
import           Test.Tiling
import           Test.Unboxed
import           Test.Utils
import           Test.Windows

import           Control.Lens          hiding (index)
import           Control.Monad         (replicateM)
import qualified Data.Align            as Align
import           Data.Maybe            (isNothing)
import           Data.Functor.Rep
import qualified Data.Finitary          as F
import           Data.Monoid           (Sum)
import qualified Data.These            as These
import qualified Data.Universe.Class  as U
import qualified Data.Ix               as Ix
import           Data.Proxy
import qualified Data.Zip              as Zip
import           GHC.TypeLits
import           Test.QuickCheck       (Arbitrary (..), Property, property,
                                        (.&&.), (===))
import           Test.QuickCheck.Classes (applicativeLaws, applyLaws,
                                          boundedEnumLaws,
                                          commutativeMonoidLaws, enumLaws,
                                          eqLaws, foldableLaws, functorLaws,
                                          genericLaws, jsonLaws, monadLaws,
                                          ixLaws, ordLaws, semigroupMonoidLaws,
                                          showLaws, traversableLaws)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

assertOrderd :: Ord a => [a] -> Assertion
assertOrderd =
    let helper []     = True
        helper (x:xs) = all (x <=) xs && helper xs
     in assertBool "Ordered" . helper

universeTests :: TestTree
universeTests =
    testGroup
        "Universe and Finite"
        [ testCase "Coord uses allCoord" $
          assertEqual
              "Coord universe"
              (allCoord @'[Ordinal 2, Ordinal 3])
              (U.universe @(Coord '[Ordinal 2, Ordinal 3]))
        , testCase "Coord finite universe uses allCoord" $
          assertEqual
              "Coord finite universe"
              (allCoord @'[Ordinal 2, Ordinal 3])
              (U.universeF @(Coord '[Ordinal 2, Ordinal 3]))
        , testCase "Coord finitary enumeration uses allCoord" $
          assertEqual
              "Coord finitary inhabitants"
              (allCoord @'[Ordinal 2, Ordinal 3])
              (F.inhabitants @(Coord '[Ordinal 2, Ordinal 3]))
        , testCase "Coord finitary indices are row-major" $
          assertEqual
              "Coord finitary indices"
              ([0 .. 5] :: [Integer])
              (map (fromIntegral . F.toFinite)
                   (allCoord @'[Ordinal 2, Ordinal 3]))
        , testCase "axis uses allCoordLike" $
          assertEqual
              "axis universe"
              (allCoordLike @5 @Periodic)
              (U.universe @(Periodic 5))
        ]

semialignZipTests :: TestTree
semialignZipTests =
    let first = tabulateGrid coordPosition :: Grid '[Periodic 3, Periodic 4] Int
        second = mapGrid (* 10) first
    in testGroup
         "Semialign and Zip"
         [ testCase "alignWith is pointwise" $
           assertEqual
               "aligned grid"
               (zipWithGrid (+) first second)
               (Align.alignWith combine first second)
         , testCase "zipWith is pointwise" $
           assertEqual
               "zipped grid"
               (zipWithGrid (+) first second)
               (Zip.zipWith (+) first second)
         , testProperty "alignWith is pointwise for arbitrary grids" $
           property $ \(left :: Grid '[Periodic 3, Periodic 4] Int)
                          (right :: Grid '[Periodic 3, Periodic 4] Int) ->
             Align.alignWith combine left right === zipWithGrid (+) left right
         , testProperty "align is pointwise These" $
           property $ \(left :: Grid '[Periodic 3, Periodic 4] Int)
                          (right :: Grid '[Periodic 3, Periodic 4] Int) ->
             Align.align left right === Zip.zipWith These.These left right
         , testProperty "Zip agrees with zipWithGrid for arbitrary grids" $
           property $ \(left :: Grid '[Periodic 3, Periodic 4] Int)
                          (right :: Grid '[Periodic 3, Periodic 4] Int) ->
             Zip.zipWith (+) left right === zipWithGrid (+) left right
         ]
  where
    combine (These.This value) = value
    combine (These.That value) = value
    combine (These.These left right) = left + right

testAllCoordOrdered ::
       forall cs proxy. IsCoordList cs
    => proxy (Coord cs)
    -> TestTree
testAllCoordOrdered _ =
    testCase "allCoord is ordered" $ assertOrderd (allCoord @cs)

-- | The row-major layout `coordPosition` defines, and the inverse that
-- undoes it. Both `index` and `tabulate` rest on this being a bijection
-- between @[0, coordSpaceSize)@ and the coordinates taken in `allCoord`
-- order.
testCoordLayout ::
       forall cs proxy.
      (All Show cs, All Arbitrary cs, All Bounded cs, IsCoordList cs)
    => proxy (Coord cs)
    -> TestTree
testCoordLayout _ =
    testGroup
        "Positions and coordinates are a bijection"
        [ testCase "coordPosition numbers allCoord from zero upwards" $
          assertEqual
              "positions"
              [0 .. coordSpaceSize @cs - 1]
              (map coordPosition (allCoord @cs))
        , testCase "coordSpaceSize counts the coordinates" $
          assertEqual "size" (coordSpaceSize @cs) (length (allCoord @cs))
        , testProperty "coordFromPosition undoes coordPosition" $
          property $ \(c :: Coord cs) ->
              coordFromPosition (coordPosition c) === Just c
        , testProperty "Ord agrees with Enum positions" $
          property $ \(a :: Coord cs) (b :: Coord cs) ->
              compare a b === compare (fromEnum a) (fromEnum b)
        , testProperty "bounded Enum is allCoord in order" $
          property $ [minBound .. maxBound] === allCoord @cs
        , testCase "coordFromPosition rejects positions outside the space" $ do
            assertBool "negative" $ isNothing (coordFromPosition @cs (-1))
            assertBool "one past the end" $
                isNothing (coordFromPosition @cs (coordSpaceSize @cs))
        ]

coordIxTests :: TestTree
coordIxTests =
    testCase "coordinate Ix uses componentwise sub-boxes" $
      case ( coordFromPosition 27 :: Maybe (Coord '[Clamped 10, Periodic 20])
           , coordFromPosition 69 :: Maybe (Coord '[Clamped 10, Periodic 20])
           , coordFromPosition 48 :: Maybe (Coord '[Clamped 10, Periodic 20])
           , coordFromPosition 50 :: Maybe (Coord '[Clamped 10, Periodic 20])
           ) of
        (Just lower, Just upper, Just insideCoord, Just outsideCoord) -> do
          assertEqual "range" [27, 28, 29, 47, 48, 49, 67, 68, 69]
            (map coordPosition (Ix.range (lower, upper)))
          assertEqual "indices" [0 .. 8]
            (map (Ix.index (lower, upper)) (Ix.range (lower, upper)))
          assertEqual "rangeSize" 9 (Ix.rangeSize (lower, upper))
          assertBool "inside" (Ix.inRange (lower, upper) insideCoord)
          assertBool "outside" (not (Ix.inRange (lower, upper) outsideCoord))
        _ -> assertFailure "test coordinates could not be constructed"

gridTests ::
       forall cs a x y f g.
       ( Show (Coord cs)
       , IsCoordList cs
       , AllSizedKnown cs
       , Show a
       , Eq a
       , cs ~ '[ f x, g y]
       , Arbitrary a
       , Arbitrary (f x)
       , Arbitrary (g y)
       )
    => Proxy (Coord cs)
    -> Proxy a
    -> [TestTree]
gridTests _genC _genA =
  let tabulateIndex :: Coord cs -> Property
      tabulateIndex c = c === index (tabulate id :: Grid cs (Coord cs)) c
      numIsPointwise :: Grid cs Int -> Grid cs Int -> Property
      numIsPointwise a b =
        (a + b === zipWithGrid (+) a b) .&&.
        (a * b === zipWithGrid (*) a b) .&&.
        (negate a === mapGrid negate a) .&&.
        (abs a === mapGrid abs a) .&&.
        (signum a === mapGrid signum a) .&&.
        ((3 :: Grid cs Int) === pure 3)
      collapseUnCollapse :: Property
      collapseUnCollapse =
        property $ do
          g :: Grid cs a <- sequenceA $ pure arbitrary
          return (Just g === gridFromList (collapseGrid g))
      uncollapseCollapse =
        property $ do
          cg :: [[a]] <-
            replicateM (fromIntegral $ natVal (Proxy @x)) $
            replicateM (fromIntegral $ natVal (Proxy @y)) arbitrary
          -- The annotation is load-bearing: 'gridFromList' is polymorphic in
          -- the vector and this round trip leaves nothing else to infer it from.
          return
            (Just cg ===
             (collapseGrid <$> (gridFromList cg :: Maybe (Grid cs a))))
      doubleTranspose =
        property $ do
          g :: Grid cs a <- sequenceA $ pure arbitrary
          return (g === transposeGrid (transposeGrid g))
      imapIsTabulate =
        property $ do
          g :: Grid cs a <- sequenceA $ pure arbitrary
          return (imap const g === (tabulate id :: Grid cs (Coord cs)))
      indexedFoldsAgree =
        property $ do
          g :: Grid cs a <- sequenceA $ pure arbitrary
          return $
            (ifoldMap (\c _ -> [c]) g === allCoord @cs) .&&.
            (ifoldr (\c _ acc -> c : acc) [] g === allCoord @cs) .&&.
            (reverse (ifoldl' (\c acc _ -> c : acc) [] g) === allCoord @cs)
  in [ testProperty "Tabulate index" tabulateIndex
     , testProperty "Collapse UnCollapse" collapseUnCollapse
     , testProperty "UnCollapse and Collapse" uncollapseCollapse
     , testProperty "Transpose twice is id" doubleTranspose
     , testProperty "imap indexes every cell by its own coord" imapIsTabulate
     , testProperty "ifoldMap, ifoldr and ifoldl' agree" indexedFoldsAgree
    , testProperty "Num operations are pointwise" numIsPointwise
     ]

splitTests ::
       forall c x cs a.
       ( Show a
       , Eq a
       , AllSizedKnown cs
       , AllSizedKnown '[c x]
       , AllSizedKnown (c x : cs)
       , Arbitrary a
       )
    => Proxy (c x ': cs)
    -> Proxy a
    -> [TestTree]
splitTests _ _ =
  let splitAndCombine =
        property $ do
          g :: Grid (c x ': cs) a <- sequenceA $ pure arbitrary
          return (g === combineGrid (splitGrid g))
      combineAndSplit =
        property $ do
          g :: Grid '[ c x] (Grid cs a) <-
            sequenceA $ pure (sequenceA $ pure arbitrary)
          return (g === splitGrid (combineGrid g))
      higherSplitAndCombine =
        property $ do
          g :: Grid (Ordinal 5 ': cs) a <- sequenceA $ pure arbitrary
          let (a :: Grid (Ordinal 3 ': cs) a, b) = splitHigherDim g
          return (g === combineHigherDim a b)
      higherCombineAndSplit =
        property $ do
          g1 :: Grid (Ordinal 3 ': cs) a <- sequenceA $ pure arbitrary
          g2 :: Grid (Ordinal 2 ': cs) a <- sequenceA $ pure arbitrary
          let g = combineHigherDim g1 g2
          return ((g1, g2) === splitHigherDim g)
  in [ testProperty "Split and Combine" splitAndCombine
     , testProperty "Combine and split" combineAndSplit
     , testProperty "Split and Combine Higher dim" higherSplitAndCombine
     , testProperty "Combine and Split Higher dim" higherCombineAndSplit
     ]

twoDimensionalCoordTests ::
     forall cs x y .
     ( cs ~ '[ x, y]
     , IsCoordLifted x
     , IsCoordLifted y
     , All Show cs
     , All Arbitrary cs
     )
  => Proxy (Coord cs)
  -> [TestTree]
twoDimensionalCoordTests _ =
  let doubleTranspose :: Coord cs -> Property
      doubleTranspose c = c === tranposeCoord (tranposeCoord c)
  in [testProperty "Transpose twice is id" doubleTranspose]

coordCreationTests ::
     forall cs a c.
     ( IsCoordLifted a
     , IsCoordLifted c
     , IsCoordList cs
     , All Show cs
     , Eq a
     , Show a
     , Show c
     , Arbitrary a
     , All Arbitrary cs
     , Arbitrary c
     )
  => Proxy (Coord (c ': cs))
  -> Proxy a
  -> [TestTree]
coordCreationTests _genC _gen =
  [ testProperty "Create single coord" $
    property $ \(g :: a) -> g === singleCoord g ^. _1
  , testProperty "Create double coord" $
    property $ \(a :: a) (b :: a) ->
      let coord = b :| singleCoord a
      in (a === coord ^. _2) .&&. (b === coord ^. _1)
  , testProperty "Create triple coord" $
    property $ \(a :: a) (b :: a) (c :: a) ->
      let coord = c :| (b :| singleCoord a)
      in (a === coord ^. _3) .&&. (b === coord ^. _2) .&&. (c === coord ^. _1)
  , testProperty "Head and append" $
    property $ \(coord :: Coord (c ': cs)) (a :: a) ->
      let newCoord = appendCoord a coord
      in (a === newCoord ^. coordHead) .&&. (coord === newCoord ^. coordTail)
  , testProperty "Tail destruction" $
    property $ \(coord :: Coord (c ': cs)) ->
      appendCoord (coord ^. coordHead) (coord ^. coordTail) === coord
  ]

main :: IO ()
main =
  let ordinal =
        [ isCoordLaws @Ordinal @10
        , lawsToTest $ ixLaws (Proxy @(Ordinal 10))
        , lawsToTest $ showLaws (Proxy @(Ordinal 10))
        , jsonKeyLaws @(Ordinal 10)
        ]
      periodic =
        [ semigroupLaws @(Periodic 10)
        , lawsToTest $ ixLaws (Proxy @(Periodic 10))
        , monoidLaws @(Periodic 10)
        , lawsToTest $ commutativeMonoidLaws (Proxy @(Periodic 10))
        , lawsToTest $ semigroupMonoidLaws (Proxy @(Periodic 10))
        , lawsToTest $ boundedEnumLaws (Proxy @(Periodic 10))
        , lawsToTest $ enumLaws (Proxy @(Periodic 10))
        , additiveGroupLaws @(Periodic 10)
        , groupLaws @(Periodic 10)
        , abelianLaws @(Periodic 10)
        , affineSpaceLaws @(Periodic 10)
        , aesonLaws @(Periodic 10)
        , lawsToTest $ jsonLaws (Proxy @(Periodic 10))
        , jsonKeyLaws @(Periodic 10)
        , isCoordLaws @Periodic @10
        ]
      clamped =
        [ semigroupLaws @(Clamped 10)
        , lawsToTest $ ixLaws (Proxy @(Clamped 10))
        , monoidLaws @(Clamped 10)
        , lawsToTest $ commutativeMonoidLaws (Proxy @(Clamped 10))
        , lawsToTest $ semigroupMonoidLaws (Proxy @(Clamped 10))
        , affineSpaceLaws @(Clamped 10)
        , aesonLaws @(Clamped 10)
        , lawsToTest $ jsonLaws (Proxy @(Clamped 10))
        , jsonKeyLaws @(Clamped 10)
        , isCoordLaws @Clamped @10
        ]
      -- 'Reflective' and 'Reflect101' have no 'Semigroup'\/'Monoid' instance;
      -- 'Test.Reflective' has the bounce-specific properties.
      reflective =
        [ affineSpaceLaws @(Reflective 10)
        , lawsToTest $ ixLaws (Proxy @(Reflective 10))
        , aesonLaws @(Reflective 10)
        , lawsToTest $ jsonLaws (Proxy @(Reflective 10))
        , jsonKeyLaws @(Reflective 10)
        , isCoordLaws @Reflective @10
        ]
      reflect101 =
        [ affineSpaceLaws @(Reflect101 10)
        , lawsToTest $ ixLaws (Proxy @(Reflect101 10))
        , aesonLaws @(Reflect101 10)
        , lawsToTest $ jsonLaws (Proxy @(Reflect101 10))
        , jsonKeyLaws @(Reflect101 10)
        , isCoordLaws @Reflect101 @10
        ]
      coord =
        [ semigroupLaws @(Coord '[ Clamped 10, Periodic 20])
        , monoidLaws @(Coord '[ Clamped 10, Periodic 20])
        , lawsToTest $
          commutativeMonoidLaws (Proxy @(Coord '[ Clamped 10, Periodic 20]))
        , lawsToTest $
          semigroupMonoidLaws (Proxy @(Coord '[ Clamped 10, Periodic 20]))
        , affineSpaceLaws @(Coord '[ Clamped 10, Periodic 20])
        , aesonLaws @(Coord '[ Clamped 10, Periodic 20])
        , lawsToTest $ jsonLaws (Proxy @(Coord '[ Clamped 10, Periodic 20]))
        , lawsToTest $ boundedEnumLaws (Proxy @(Coord '[ Clamped 10, Periodic 20]))
        , testAllCoordOrdered (Proxy @(Coord '[ Clamped 10, Periodic 20]))
        , testCoordLayout (Proxy @(Coord '[ Clamped 10, Periodic 20]))
        , isoLaws
            "_WrappedCoord"
            (_WrappedCoord @'[ Clamped 10, Periodic 20])
        , lawsToTest $ eqLaws (Proxy @(Coord '[ Clamped 10, Periodic 20]))
        , lawsToTest $ ordLaws (Proxy @(Coord '[ Clamped 10, Periodic 20]))
        , lawsToTest $ showLaws (Proxy @(Coord '[ Clamped 10, Periodic 20]))
        ]
      coord2 =
        [ semigroupLaws @(Coord '[ Periodic 10, Periodic 20])
        , monoidLaws @(Coord '[ Periodic 10, Periodic 20])
        , lawsToTest $
          commutativeMonoidLaws (Proxy @(Coord '[ Periodic 10, Periodic 20]))
        , lawsToTest $
          semigroupMonoidLaws (Proxy @(Coord '[ Periodic 10, Periodic 20]))
        , affineSpaceLaws @(Coord '[ Periodic 10, Periodic 20])
        , additiveGroupLaws @(Coord '[ Periodic 10, Periodic 20])
        , groupLaws @(Coord '[ Periodic 10, Periodic 20])
        , abelianLaws @(Coord '[ Periodic 10, Periodic 20])
        , aesonLaws @(Coord '[ Periodic 10, Periodic 20])
        , lawsToTest $ jsonLaws (Proxy @(Coord '[ Periodic 10, Periodic 20]))
        , lawsToTest $ boundedEnumLaws (Proxy @(Coord '[ Periodic 10, Periodic 20]))
        , testAllCoordOrdered (Proxy @(Coord '[ Periodic 10, Periodic 20]))
        , testCoordLayout (Proxy @(Coord '[ Periodic 10, Periodic 20]))
        , lawsToTest $ eqLaws (Proxy @(Coord '[ Periodic 10, Periodic 20]))
        , lawsToTest $ ordLaws (Proxy @(Coord '[ Periodic 10, Periodic 20]))
        , lawsToTest $ showLaws (Proxy @(Coord '[ Periodic 10, Periodic 20]))
        ]
  in defaultMain $
     testGroup
       "tests"
       [ testGroup "Ordinal 10" ordinal
      , universeTests
      , semialignZipTests
       , testGroup "Periodic 10" periodic
       , testGroup "Clamped 10" clamped
       , testGroup "Reflective 10" reflective
       , testGroup "Reflect101 10" reflect101
       , reflectiveTests
       , testGroup "Coord [Clamped 10, Periodic 20]" coord
       , testGroup "Coord [Periodic 10, Periodic 20]" coord2
      , coordIxTests
       , testGroup "2D Coords" $
         twoDimensionalCoordTests (Proxy @(Coord '[ Clamped 10, Periodic 10]))
         -- The element proxy is an axis type, not 'Int'. It used to be able to
         -- be anything, because a coord was an @NP@ that would hold it; a
         -- coord is a row-major position now, so @Coord '[Int]@ has no sizes
         -- to be a position within and the tests below build real axes.
       , testGroup
           "Coord creation"
           (coordCreationTests
              (Proxy @(Coord '[ Clamped 10, Periodic 10]))
              (Proxy @(Clamped 10)))
       , testGroup
           "Grid"
           (gridTests
               (Proxy @(Coord '[ Periodic 10, Periodic 11]))
               (Proxy @Int) ++
             map
               lawsToTest
               [ functorLaws (Proxy @(Grid '[ Periodic 10, Periodic 11]))
               , applyLaws (Proxy @(Grid '[ Periodic 10, Periodic 11]))
               , applicativeLaws (Proxy @(Grid '[ Periodic 10, Periodic 11]))
               , bindLaws @(Grid '[ Periodic 10, Periodic 11])
               , monadLaws (Proxy @(Grid '[ Periodic 10, Periodic 11]))
               , foldableLaws (Proxy @(Grid '[ Periodic 10, Periodic 11]))
               , traversableLaws (Proxy @(Grid '[ Periodic 10, Periodic 11]))
               ] ++
             [ aesonLaws @(Grid '[ Periodic 10, Periodic 11] Int)
             , lawsToTest $
               jsonLaws (Proxy @(Grid '[ Periodic 10, Periodic 11] Int))
             , lawsToTest $
               genericLaws (Proxy @(Grid '[ Periodic 10, Periodic 11] Int))
             , eq1Laws (Proxy @(Grid '[ Periodic 10, Periodic 20]))
             ] ++
             [ semigroupLaws @(Grid '[ Periodic 10, Periodic 11] (Sum Int))
             , monoidLaws @(Grid '[ Periodic 10, Periodic 11] (Sum Int))
             , lawsToTest $
               semigroupMonoidLaws
                 (Proxy @(Grid '[ Periodic 10, Periodic 11] (Sum Int)))
             ])
         -- Two transforms that do not commute with each other, so a
         -- composition that ran them in the wrong order would show up.
       , testGroup
           "axis"
           [ setterLaws
               "axis 0"
               (axis 0 ::
                  Setter'
                    (Grid '[ Periodic 10, Periodic 11] Int)
                    (Grid '[ Periodic 10] Int))
               ("scanl1Grid (+)", scanl1Grid (+))
               ("mapGrid (+ 1)", mapGrid (+ 1))
           , setterLaws
               "axis 1"
               (axis 1 ::
                  Setter'
                    (Grid '[ Periodic 10, Periodic 11] Int)
                    (Grid '[ Periodic 11] Int))
               ("scanl1Grid (+)", scanl1Grid (+))
               ("mapGrid (+ 1)", mapGrid (+ 1))
             -- The middle axis of a 3D grid: the case that reaches
             -- 'mapAxisHere' through the recursive 'MapAxis' instance.
           , setterLaws
               "axis 1 of three"
               (axis 1 ::
                  Setter'
                    (Grid '[ Clamped 2, Periodic 3, Clamped 4] Int)
                    (Grid '[ Periodic 3] Int))
               ("scanl1Grid (+)", scanl1Grid (+))
               ("mapGrid (+ 1)", mapGrid (+ 1))
           ]
       , testGroup
           "Splitting"
           (splitTests
              (Proxy @'[ Clamped 8, Clamped 3, Clamped 5])
              (Proxy @Int))
       , testGroup
           "Representable"
           [ lawsToTest $
             representableLaws @(Grid '[ Periodic 10, Periodic 11])
           , lawsToTest $
             representableLaws @(Grid '[ Clamped 3, Periodic 4, Clamped 5])
           ]
       , testGroup
           "Distributive"
           [ lawsToTest $
             distributiveLaws @(Grid '[ Periodic 3, Periodic 4])
           , lawsToTest $
             distributiveLaws @(Grid '[ Clamped 2, Periodic 3, Clamped 2])
           ]
         -- Small on purpose: coassociativity builds a grid of grids of grids,
         -- so the cell count is cubed. See 'comonadLaws'.
       , testGroup
           "FocusedGrid"
           [ lawsToTest $
             comonadLaws @(FocusedGrid '[ Periodic 3, Periodic 3])
           , lawsToTest $
             comonadLaws @(FocusedGrid '[ Clamped 2, Periodic 3])
           , isoLaws
               "_FocusedGrid"
               (_FocusedGrid ::
                  Iso'
                    (FocusedGrid '[ Periodic 10, Periodic 11] Int)
                    (Grid '[ Periodic 10, Periodic 11] Int, Coord '[ Periodic 10, Periodic 11]))
           , lensLaws
               "focus"
               (focus ::
                  Lens'
                    (FocusedGrid '[ Periodic 10, Periodic 11] Int)
                    (Coord '[ Periodic 10, Periodic 11]))
           , lensLaws
               "unfocused"
               (unfocused ::
                  Lens'
                    (FocusedGrid '[ Periodic 10, Periodic 11] Int)
                    (Grid '[ Periodic 10, Periodic 11] Int))
           , lensLaws
               "asGrid on FocusedGrid"
               (asGrid ::
                  Lens'
                    (FocusedGrid '[ Periodic 10, Periodic 11] Int)
                    (Grid '[ Periodic 10, Periodic 11] Int))
           , lensLaws
               "asGrid on Grid"
               (asGrid ::
                  Lens'
                    (Grid '[ Periodic 10, Periodic 11] Int)
                    (Grid '[ Periodic 10, Periodic 11] Int))
           ]
       , axisTests
       , shrinkTests
      , sliceTests
       , stencilTests
       , tilingTests
       , unboxedTests
       , windowTests
       , neighbourTests
       , boundaryTests
       , rayTests
       , pathTests
       , focusedTests
      , cellTests
       , invariantTests
       , ordinalTests
       , periodicTests
       , compileFailTests
       ]
