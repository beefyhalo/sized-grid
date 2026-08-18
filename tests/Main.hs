module Main
  ( main
  ) where

import           Data.Grid.Sized

import           Test.Arbitrary ()
import           Test.Boundary
import           Test.CompileFail
import           Test.Focused
import           Test.Invariant
import           Test.Neighbours
import           Test.Ordinal
import           Test.Path
import           Test.Ray
import           Test.Reflective
import           Test.Shrink
import           Test.Stencil
import           Test.Tiling
import           Test.Unboxed
import           Test.Utils
import           Test.Windows

import           Control.Lens          hiding (index)
import           Control.Monad         (replicateM)
import           Data.Maybe            (isNothing)
import           Data.Functor.Rep
import           Data.Proxy
import           GHC.TypeLits
import           Test.QuickCheck       (Arbitrary (..), Property, property,
                                        (.&&.), (===))
import           Test.QuickCheck.Classes (applicativeLaws, foldableLaws,
                                          functorLaws, monadLaws,
                                          traversableLaws)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

assertOrderd :: Ord a => [a] -> Assertion
assertOrderd =
    let helper []     = True
        helper (x:xs) = all (x <=) xs && helper xs
     in assertBool "Ordered" . helper

testAllCoordOrdered ::
       forall cs proxy. (All Eq cs, All Ord cs, IsCoordList cs)
    => proxy (Coord cs)
    -> TestTree
testAllCoordOrdered _ =
    testCase "allCoord is ordered" $ assertOrderd (allCoord @cs)

-- | The row-major layout `coordPosition` defines, and the inverse that undoes
-- it. Both `index` and `tabulate` rest on this being a bijection between
-- @[0, coordSpaceSize)@ and the coordinates taken in `allCoord` order, and
-- nothing checked that: `coordPosition` used to fold the axis sizes back up on
-- every call, so a mistake in the stride would have been invisible until a grid
-- read the wrong cell.
testCoordLayout ::
       forall cs proxy.
       (All Eq cs, All Show cs, All Arbitrary cs, IsCoordList cs)
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
        , testCase "coordFromPosition rejects positions outside the space" $ do
            assertBool "negative" $ isNothing (coordFromPosition @cs (-1))
            assertBool "one past the end" $
                isNothing (coordFromPosition @cs (coordSpaceSize @cs))
        ]

gridTests ::
       forall cs a x y f g.
       ( Show (Coord cs)
       , Eq (Coord cs)
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
          -- The annotation is load-bearing. 'gridFromList' is polymorphic in
          -- the vector, and a grid built only to be collapsed straight back
          -- leaves nothing to infer it from -- this round trip is the one shape
          -- of call where the representation has to be named.
          return
            (Just cg ===
             (collapseGrid <$> (gridFromList cg :: Maybe (Grid cs a))))
      doubleTranspose =
        property $ do
          g :: Grid cs a <- sequenceA $ pure arbitrary
          return (g === transposeGrid (transposeGrid g))
      -- Each indexed traversal hands a cell the coordinate that `index` would
      -- read it back by, and all of them agree on the order. `ifoldr` and
      -- `ifoldl'` are given explicitly by the instance rather than derived from
      -- `ifoldMap`, so they need saying separately.
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
     forall cs x y . (cs ~ '[ x, y], All Show cs, All Eq cs, All Arbitrary cs)
  => Proxy (Coord cs)
  -> [TestTree]
twoDimensionalCoordTests _ =
  let doubleTranspose :: Coord cs -> Property
      doubleTranspose c = c === tranposeCoord (tranposeCoord c)
  in [testProperty "Transpose twice is id" doubleTranspose]

coordCreationTests ::
     forall cs a c.
     ( All Show cs
     , All Eq cs
     , Eq a
     , Show a
     , Show c
     , Eq c
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
  let -- 'Ordinal' is the type the other two coords are newtypes over, and the
      -- only 'IsCoord' instance that overrides 'asOrdinal', 'zeroPosition',
      -- 'maxCoord' and 'reifyCoord' all at once -- 'Periodic' and 'Clamped'
      -- take the defaults for the last three. It is therefore the instance
      -- where the two routes to a value have the most room to drift apart, and
      -- it had no 'isCoordLaws' call at all.
      ordinal = [isCoordLaws @Ordinal @10]
      periodic =
        [ semigroupLaws @(Periodic 10)
        , monoidLaws @(Periodic 10)
        , additiveGroupLaws @(Periodic 10)
        , affineSpaceLaws @(Periodic 10)
        , aesonLaws @(Periodic 10)
        , isCoordLaws @Periodic @10
        ]
      clamped =
        [ semigroupLaws @(Clamped 10)
        , monoidLaws @(Clamped 10)
        , affineSpaceLaws @(Clamped 10)
        , aesonLaws @(Clamped 10)
        , isCoordLaws @Clamped @10
        ]
      -- 'Reflective' and 'Reflect101' have no 'Semigroup'\/'Monoid' instance:
      -- unlike 'Clamped' and 'Periodic', neither type gives '<>' a meaning the
      -- issue asked for, so none was invented. 'Test.Reflective' has the
      -- bounce-specific properties; these are the same three law suites every
      -- other axis type gets here.
      reflective =
        [ affineSpaceLaws @(Reflective 10)
        , aesonLaws @(Reflective 10)
        , isCoordLaws @Reflective @10
        ]
      reflect101 =
        [ affineSpaceLaws @(Reflect101 10)
        , aesonLaws @(Reflect101 10)
        , isCoordLaws @Reflect101 @10
        ]
      coord =
        [ semigroupLaws @(Coord '[ Clamped 10, Periodic 20])
        , monoidLaws @(Coord '[ Clamped 10, Periodic 20])
        , affineSpaceLaws @(Coord '[ Clamped 10, Periodic 20])
        , aesonLaws @(Coord '[ Clamped 10, Periodic 20])
        , testAllCoordOrdered (Proxy @(Coord '[ Clamped 10, Periodic 20]))
        , testCoordLayout (Proxy @(Coord '[ Clamped 10, Periodic 20]))
          -- The library's other exported 'Iso'', and the one that shows
          -- 'isoLaws' is worth having as a helper rather than inlined into
          -- 'isCoordLaws'. Unlike 'asOrdinal' this round trip goes through
          -- the hand-written 'Eq' for 'Coord', which compares the product
          -- element by element with 'hcliftA2' rather than deriving it.
        , isoLaws
            "_WrappedCoord"
            (_WrappedCoord @'[ Clamped 10, Periodic 20])
        ]
      coord2 =
        [ semigroupLaws @(Coord '[ Periodic 10, Periodic 20])
        , monoidLaws @(Coord '[ Periodic 10, Periodic 20])
        , affineSpaceLaws @(Coord '[ Periodic 10, Periodic 20])
        , additiveGroupLaws @(Coord '[ Periodic 10, Periodic 20])
        , aesonLaws @(Coord '[ Periodic 10, Periodic 20])
        , testAllCoordOrdered (Proxy @(Coord '[ Periodic 10, Periodic 20]))
        , testCoordLayout (Proxy @(Coord '[ Periodic 10, Periodic 20]))
        ]
  in defaultMain $
     testGroup
       "tests"
       [ testGroup "Ordinal 10" ordinal
       , testGroup "Periodic 10" periodic
       , testGroup "Clamped 10" clamped
       , testGroup "Reflective 10" reflective
       , testGroup "Reflect101 10" reflect101
       , reflectiveTests
       , testGroup "Coord [Clamped 10, Periodic 20]" coord
       , testGroup "Coord [Periodic 10, Periodic 20]" coord2
       , testGroup "2D Coords" $
         twoDimensionalCoordTests (Proxy @(Coord '[ Clamped 10, Periodic 10]))
       , testGroup
           "Coord creation"
           (coordCreationTests
              (Proxy @(Coord '[ Clamped 10, Periodic 10]))
              (Proxy @Int))
       , testGroup
           "Grid"
           (gridTests
               (Proxy @(Coord '[ Periodic 10, Periodic 11]))
               (Proxy @Int) ++
             -- The base-class laws (sized-grid-05b): 'Grid' has Functor,
             -- Applicative, Monad, Foldable and Traversable instances, and
             -- before this only Applicative was tested, by hand. These come
             -- from @quickcheck-classes@ rather than 'Test.Utils' because
             -- upstream already ships exactly these five bundles; nothing here
             -- needed reinventing.
             map
               lawsToTest
               [ functorLaws (Proxy @(Grid '[ Periodic 10, Periodic 11]))
               , applicativeLaws (Proxy @(Grid '[ Periodic 10, Periodic 11]))
               , monadLaws (Proxy @(Grid '[ Periodic 10, Periodic 11]))
               , foldableLaws (Proxy @(Grid '[ Periodic 10, Periodic 11]))
               , traversableLaws (Proxy @(Grid '[ Periodic 10, Periodic 11]))
               ] ++
             [ aesonLaws @(Grid '[ Periodic 10, Periodic 11] Int)
             , eq1Laws (Proxy @(Grid '[ Periodic 10, Periodic 20]))
             ])
       , testGroup
           "Splitting"
           (splitTests
              (Proxy @'[ Clamped 8, Clamped 3, Clamped 5])
              (Proxy @Int))
         -- `Representable` is the instance the whole `Grid` API is built on:
         -- `index`, `tabulate`, `Distributive` and the `Comonad` for
         -- `FocusedGrid` all bottom out in it. The 3D case is here because
         -- `coordPosition`'s strides are only interestingly wrong past two
         -- dimensions -- with two axes a transposed stride is still a
         -- permutation of the right one on square grids.
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
           ]
       , shrinkTests
       , stencilTests
       , tilingTests
       , unboxedTests
       , windowTests
       , neighbourTests
       , boundaryTests
       , rayTests
       , pathTests
       , focusedTests
       , invariantTests
       , ordinalTests
       , compileFailTests
       ]
