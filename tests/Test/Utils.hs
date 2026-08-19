{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Test.Utils
  ( eq1Laws
  , aesonLaws
  , jsonKeyLaws
  , semigroupLaws
  , monoidLaws
  , additiveGroupLaws
  , affineSpaceLaws
  , traversalLaws
  , isoLaws
  , isCoordLaws
  , comonadLaws
  , representableLaws
  , distributiveLaws
  , lawsToTest
  ) where

import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Ordinal

-- The orphan 'Arbitrary' instances: 'isCoordLaws' needs 'Ordinal''s own, not
-- just the caller's.
import           Test.Arbitrary        ()

import           Control.Comonad
import           Control.Lens          hiding (index)
import           Data.AdditiveGroup
import           Data.Aeson
import           Data.AffineSpace
import           Data.Distributive
import           Data.Functor.Classes
import           Data.Functor.Compose
import           Data.Map              (Map)
-- 'Data.Functor.Identity' is not imported: 'Data.Functor.Rep' re-exports it.
import           Data.Functor.Rep
import           Data.Proxy
import           GHC.TypeLits
import           Test.Tasty
import           Test.Tasty.HUnit
-- Hidden because the `Representable`/`Distributive` methods of the same
-- names are wanted here instead.
import           Test.Tasty.QuickCheck hiding (collect, tabulate)
import           Test.QuickCheck.Classes (Laws (..))

eq1Laws ::
       forall f. (Eq1 f, Applicative f)
    => Proxy f
    -> TestTree
eq1Laws _ =
    let nilEq =
            assertEqual "Nil equal" True $ liftEq (==) (pure ()) (pure @f ())
    in testGroup "Eq1 Laws" [testCase "Nil Eq" nilEq]

aesonLaws ::
     forall a. (Show a, Eq a, ToJSON a, FromJSON a, Arbitrary a)
  => TestTree
aesonLaws =
  let encodeDecode :: a -> Property
      encodeDecode a = Just a === decode (encode a)
  in testGroup "Aeson Laws" [testProperty "Encode decode" encodeDecode]

-- | 'ToJSONKey'\/'FromJSONKey' round-trip. Key encoding is a different code
-- path from 'ToJSON'\/'FromJSON' -- aeson only takes it via a 'Map', so that
-- is what this routes the round trip through.
jsonKeyLaws ::
     forall a. (Show a, Ord a, ToJSONKey a, FromJSONKey a, Arbitrary a)
  => TestTree
jsonKeyLaws =
  let encodeDecode :: Map a Int -> Property
      encodeDecode m = Just m === decode (encode m)
  in testGroup
       "JSON Key Laws"
       [testProperty "Encode decode via Map key" encodeDecode]

semigroupLaws ::
     forall a. (Show a, Eq a, Semigroup a, Arbitrary a)
  => TestTree
semigroupLaws =
  let assoc :: a -> a -> a -> Property
      assoc a b c = a <> (b <> c) === (a <> b) <> c
  in testGroup "Semigroup Laws" [testProperty "Associative" assoc]

monoidLaws ::
     forall a. (Show a, Eq a, Monoid a, Arbitrary a)
  => TestTree
monoidLaws =
  let assoc :: a -> a -> a -> Property
      assoc a b c = mappend a (mappend b c) === mappend (mappend a b) c
      memptyId :: a -> Property
      memptyId a = (a === mappend mempty a) .&&. (a === mappend a mempty)
      concatIsFold :: [a] -> Property
      concatIsFold as = mconcat as === foldr mappend mempty as
  in testGroup
       "Monoid laws"
       [ testProperty "Associative" assoc
       , testProperty "Mempty Id" memptyId
       , testProperty "Concat is Fold" concatIsFold
       ]

additiveGroupLaws ::
     forall a. (Show a, Eq a, AdditiveGroup a, Arbitrary a)
  => TestTree
additiveGroupLaws =
  let assoc :: a -> a -> a -> Property
      assoc a b c = a ^+^ (b ^+^ c) === (a ^+^ b) ^+^ c
      zeroId :: a -> Property
      zeroId a = (a === zeroV ^+^ a) .&&. (a === a ^+^ zeroV)
      inverseId :: a -> Property
      inverseId a = a ^-^ a === zeroV
      takeLeaves :: a -> a -> Property
      takeLeaves a b = a ^-^ (a ^-^ b) === b
  in testGroup
       "AdditiveGroup laws"
       [ testProperty "Associative" assoc
       , testProperty "Zero Id" zeroId
       , testProperty "Inverse id is zeroV" inverseId
       , testProperty "a - (a - b) = b" takeLeaves
       ]

affineSpaceLaws ::
     forall a.
     (Arbitrary a, Show a, Eq a, AffineSpace a, Eq (Diff a), Show (Diff a))
  => TestTree
affineSpaceLaws =
  let addZero :: a -> Property
      addZero a = a === a .+^ zeroV
      takeSelf :: a -> Property
      takeSelf a = a .-. a === zeroV
      -- The law that pins down (.-.): a difference must carry you back.
      subtractThenAdd :: a -> a -> Property
      subtractThenAdd a b = a === b .+^ (a .-. b)
  in testGroup
       "AffineSpace Laws"
       [ testProperty "Add Zero" addZero
       , testProperty "Take self" takeSelf
       , testProperty "b .+^ (a .-. b) == a" subtractThenAdd
       ]

-- | Renders a @quickcheck-classes@ 'Laws' bundle as a 'TestTree'.
lawsToTest :: Laws -> TestTree
lawsToTest (Laws name props) = testGroup name (map (uncurry testProperty) props)

traversalLaws ::
     forall a f b.
     ( Eq a
     , Show a
     , Functor f
     , Arbitrary a
     , Function b
     , CoArbitrary b
     , Arbitrary b
     )
  => Traversal' a (f b)
  -> TestTree
traversalLaws t =
  let pureId =
        property $ do
          a :: a <- arbitrary
          return (pure @[] a === t pure a)
      compose =
        property $ do
          a :: a <- arbitrary
          fFunc :: b -> b <- applyFun <$> arbitrary
          gFunc :: b -> b <- applyFun <$> arbitrary
          let raiseFunc f x = Just (f <$> x)
          return
            (fmap (t (raiseFunc fFunc)) (t (raiseFunc gFunc) a) ===
             getCompose
               (t (Compose . fmap (raiseFunc fFunc) . (raiseFunc gFunc)) a))
  in testGroup
       "Traveral Laws"
       [testProperty "Pure Id" pureId, testProperty "Compose" compose]

-- | The three comonad laws for `Data.Grid.Sized.Focused.FocusedGrid`.
--
-- Coassociativity builds a grid of grids of grids, so @w@ should stay small.
comonadLaws ::
     forall w.
     ( Comonad w
     , forall a. Arbitrary a => Arbitrary (w a)
     , forall a. Show a => Show (w a)
     , forall a. Eq a => Eq (w a)
     )
  => Laws
comonadLaws =
  let leftId :: w Int -> Property
      leftId w = extract (duplicate w) === w
      rightId :: w Int -> Property
      rightId w = fmap extract (duplicate w) === w
      coassociativity :: w Int -> Property
      coassociativity w =
        duplicate (duplicate w) === fmap duplicate (duplicate w)
   in Laws
        "Comonad"
        [ ("extract . duplicate == id", property leftId)
        , ("fmap extract . duplicate == id", property rightId)
        , ( "duplicate . duplicate == fmap duplicate . duplicate"
          , property coassociativity)
        ]

-- | @tabulate@ and @index@ are inverse, in both directions.
representableLaws ::
     forall f.
     ( Representable f
     , forall a. Arbitrary a => Arbitrary (f a)
     , forall a. Show a => Show (f a)
     , forall a. Eq a => Eq (f a)
     , Arbitrary (Rep f)
     , Show (Rep f)
     )
  => Laws
representableLaws =
  let tabulateIndex :: f Int -> Property
      tabulateIndex g = tabulate (index g) === g
      indexTabulate :: f Int -> Rep f -> Property
      indexTabulate g r = index (tabulate (index g) :: f Int) r === index g r
      -- A `fmap` that reordered or dropped cells would pass both laws above.
      fmapIsTabulate :: Fun Int Int -> f Int -> Property
      fmapIsTabulate h g =
        fmap (applyFun h) g === tabulate (applyFun h . index g)
   in Laws
        "Representable"
        [ ("tabulate . index == id", property tabulateIndex)
        , ("index . tabulate == id", property indexTabulate)
        , ("fmap h == tabulate . (h .) . index", property fmapIsTabulate)
        ]

-- | The `Data.Distributive.Distributive` laws.
distributiveLaws ::
     forall f.
     ( Distributive f
     , forall a. Arbitrary a => Arbitrary (f a)
     , forall a. Show a => Show (f a)
     , forall a. Eq a => Eq (f a)
     )
  => Laws
distributiveLaws =
  let doubleDistribute :: f (f Int) -> Property
      doubleDistribute g = distribute (distribute g) === g
      distributeIsCollectId :: [f Int] -> Property
      distributeIsCollectId gs = distribute gs === collect id gs
      collectIsDistributeFmap :: Fun Int (f Int) -> [Int] -> Property
      collectIsDistributeFmap h xs =
        collect (applyFun h) xs === distribute (fmap (applyFun h) xs)
      -- Distributing over 'Identity' can only be 'fmap Identity'.
      identityLaw :: f Int -> Property
      identityLaw g = fmap runIdentity (distribute (Identity g)) === g
   in Laws
        "Distributive"
        [ ("distribute . distribute == id", property doubleDistribute)
        , ("distribute == collect id", property distributeIsCollectId)
        , ("collect f == distribute . fmap f", property collectIsDistributeFmap)
        , ("distribute . Identity == fmap Identity", property identityLaw)
        ]

-- | The two round trips of an 'Control.Lens.Iso''.
isoLaws ::
     (Eq s, Show s, Arbitrary s, Eq a, Show a, Arbitrary a)
  => String -- ^ The iso's name, used to build the test labels.
  -> AnIso' s a
  -> TestTree
isoLaws name i =
  withIso i $ \there back ->
    testGroup
      (name ++ " is an isomorphism")
      [ testProperty ("review " ++ name ++ " . view " ++ name ++ " == id") $
        \s -> back (there s) === s
      , testProperty ("view " ++ name ++ " . review " ++ name ++ " == id") $
        \a -> there (back a) === a
      ]

-- | The laws of the coordinate abstraction itself. If 'asOrdinal''s two
-- directions disagree, a coord silently reads the wrong cell, everywhere.
isCoordLaws ::
     forall c n.
     ( IsCoord c
     , 1 <= n
     , KnownNat n
     , Eq (c n)
     , Show (c n)
     , Arbitrary (c n)
     )
  => TestTree
isCoordLaws =
  let -- Note what this does /not/ cover: 'arbitrary' draws through
      -- 'unsafeOrdinal' at an already-reduced index, so a coord built by a
      -- wrong operation (a bad @mod@, a bad clamp) is never reached here.
      inRange :: c n -> Property
      inRange c =
        let i = ordinalToInt (view asOrdinal c)
         in counterexample ("position " ++ show i) $
            (0 <= i) .&&. (i < ordinalSize @n)
      -- 'Ordinal' overrides the 'reifyCoord' default with 'reifyOrdinal', so
      -- the two routes agreeing is a real obligation, not a tautology.
      reifyAgrees :: c n -> Property
      reifyAgrees c =
        reifyCoord c (\m -> natVal (Proxy @m)) ===
        toInteger (ordinalToInt (view asOrdinal c))
   in testGroup
        "IsCoord Laws"
        [ testCase "distinguished values" $ do
            assertEqual
              "zeroPosition is Zero"
              (0 :: Int)
              (ordinalToNum $ view asOrdinal (zeroPosition @c @n))
            assertEqual
              "reifyCoord of zeroPosition is 0"
              (0 :: Integer)
              (reifyCoord (zeroPosition @c @n) (\m -> natVal (Proxy @m)))
            assertEqual
              "Max size equality"
              (ordinalToNum $ view (asOrdinal @c) (maxCoord :: c n))
              (maxCoordSize n)
          -- @review . view@ is the direction with teeth: an instance can still
          -- be non-injective, dropping a field on the way out and inventing it
          -- on the way back, collapsing two coords onto one ordinal.
        , isoLaws "asOrdinal" (asOrdinal :: Iso' (c n) (Ordinal n))
        , testProperty "asOrdinal lands in [0, n)" inRange
        , testProperty "reifyCoord agrees with asOrdinal" reifyAgrees
        ]

