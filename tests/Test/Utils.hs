{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Utils
  ( eq1Laws
  , aesonLaws
  , semigroupLaws
  , monoidLaws
  , additiveGroupLaws
  , affineSpaceLaws
  , applicativeLaws
  , traversalLaws
  , isCoordLaws
  , comonadLaws
  , representableLaws
  , distributiveLaws
  ) where

import           SizedGrid.Coord.Class
import           SizedGrid.Ordinal

import           Control.Comonad
import           Control.Lens          hiding (index)
import           Data.AdditiveGroup
import           Data.Aeson
import           Data.AffineSpace
import           Data.Distributive
import           Data.Functor.Classes
import           Data.Functor.Compose
-- 'Data.Functor.Identity' is not imported: 'Data.Functor.Rep' re-exports it.
import           Data.Functor.Rep
import           Data.Proxy
import           GHC.TypeLits
import           Test.Tasty
import           Test.Tasty.HUnit
-- QuickCheck's 'tabulate' and 'collect' label test cases for the statistics it
-- prints; the ones wanted here are the `Representable` and
-- `Data.Distributive.Distributive` methods of the same names.
import           Test.Tasty.QuickCheck hiding (collect, tabulate)

eq1Laws ::
       forall f. (Eq1 f, Applicative f)
    => Proxy f
    -> TestTree
eq1Laws _ =
    let nilEq =
            assertEqual "Nil equal" True $ liftEq (==) (pure ()) (pure @f ())
    in testGroup "Eq1 Laws" [testCase "Nil Eq" nilEq]

aesonLaws ::
     forall a proxy. (Show a, Eq a, ToJSON a, FromJSON a, Arbitrary a)
  => proxy a
  -> TestTree
aesonLaws _ =
  let encodeDecode :: a -> Property
      encodeDecode a = Just a === decode (encode a)
  in testGroup "Aeson Laws" [testProperty "Encode decode" encodeDecode]

semigroupLaws ::
     forall a proxy. (Show a, Eq a, Semigroup a, Arbitrary a)
  => proxy a
  -> TestTree
semigroupLaws _ =
  let assoc :: a -> a -> a -> Property
      assoc a b c = a <> (b <> c) === (a <> b) <> c
  in testGroup "Semigroup Laws" [testProperty "Associative" assoc]

monoidLaws ::
     forall a proxy. (Show a, Eq a, Monoid a, Arbitrary a)
  => proxy a
  -> TestTree
monoidLaws _ =
  let assoc :: a -> a -> a -> Property
      assoc a b c = mappend a (mappend b c) === mappend (mappend a b) c
      memptyId :: a -> Property
      memptyId a = (a === (mappend mempty a)) .&&. ((a === mappend a mempty))
      concatIsFold :: [a] -> Property
      concatIsFold as = mconcat as === foldr mappend mempty as
  in testGroup
       "Monoid laws"
       [ testProperty "Associative" assoc
       , testProperty "Mempty Id" memptyId
       , testProperty "Concat is Fold" concatIsFold
       ]

additiveGroupLaws ::
     forall a proxy. (Show a, Eq a, AdditiveGroup a, Arbitrary a)
  => proxy a
  -> TestTree
additiveGroupLaws _ =
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
     forall a proxy.
     (Arbitrary a, Show a, Eq a, AffineSpace a, Eq (Diff a), Show (Diff a))
  => proxy a
  -> TestTree
affineSpaceLaws _ =
  let addZero :: a -> Property
      addZero a = a === a .+^ zeroV
      takeSelf :: a -> Property
      takeSelf a = a .-. a === zeroV
      -- The law that actually pins down (.-.): a difference must be able to
      -- carry you back. Clamping (.-.) satisfies both laws above but not this
      -- one, which is how Clamped's clamped subtraction went unnoticed.
      subtractThenAdd :: a -> a -> Property
      subtractThenAdd a b = a === b .+^ (a .-. b)
  in testGroup
       "AffineSpace Laws"
       [ testProperty "Add Zero" addZero
       , testProperty "Take self" takeSelf
       , testProperty "b .+^ (a .-. b) == a" subtractThenAdd
       ]

applicativeLaws ::
     forall f a.
     ( Applicative f
     , Show (f a)
     , Eq (f a)
     , Arbitrary a
     , Arbitrary1 f
     , Function a
     , CoArbitrary a
     )
  => Proxy f
  -> Proxy a
  -> TestTree
applicativeLaws _ _ =
  let identiy :: Gen Property
      identiy = do
        v :: f a <- liftArbitrary arbitrary
        return (v === (pure id <*> v))
      homomorphism = do
        x :: a <- arbitrary
        f :: (a -> a) <- applyFun <$> arbitrary
        return ((pure f <*> pure x) === pure @f (f x))
      interchange :: Gen Property
      interchange = do
        u :: f (a -> a) <- liftArbitrary (applyFun <$> arbitrary)
        y :: a <- arbitrary
        let lhs :: f a = u <*> pure y
            rhs :: f a = pure ($ y) <*> u
        return (lhs === rhs)
      fmapLaw = do
        f :: (a -> a) <- applyFun <$> arbitrary
        x :: f a <- liftArbitrary arbitrary
        return ((f <$> x) === (pure f <*> x))
      composition = do
        u :: f (a -> a) <- liftArbitrary (applyFun <$> arbitrary)
        v :: f (a -> a) <- liftArbitrary (applyFun <$> arbitrary)
        w :: f a <- liftArbitrary arbitrary
        let lhs = u <*> (v <*> w)
            rhs = pure (.) <*> u <*> v <*> w
        return (lhs === rhs)
  in testGroup
       "Applicative Laws"
       [ testProperty "Identity" (property identiy)
       , testProperty "Homomorphism" (property homomorphism)
       , testProperty "Interchange" (property interchange)
       , testProperty "Fmap Law" (property fmapLaw)
       , testProperty "Composiiton" (property composition)
       ]

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

-- | The three comonad laws, for `SizedGrid.Grid.Focused.FocusedGrid`.
--
-- Its `Control.Comonad.Comonad` instance is the whole reason the type exists --
-- it is what a cellular automaton step is written against -- and nothing tested
-- it. @duplicate@ is a one-line @tabulate (FocusedGrid g)@, and the way for
-- that line to be wrong is to lose the focus or to rebuild it from the wrong
-- grid, which is precisely what the first and third laws below catch.
--
-- Coassociativity is the expensive one: it builds a grid of grids of grids, so
-- @w@ wants to be small. On a 3x3 that is 729 cells per sample; on the
-- @Periodic 10, Periodic 11@ used elsewhere in this suite it would be 1.3
-- million.
comonadLaws ::
     forall w a proxy.
     ( Comonad w
     , Arbitrary (w a)
     , Show (w a)
     , Eq (w a)
     , Show (w (w (w a)))
     , Eq (w (w (w a)))
     )
  => proxy (w a)
  -> TestTree
comonadLaws _ =
  let leftId :: w a -> Property
      leftId w = extract (duplicate w) === w
      rightId :: w a -> Property
      rightId w = fmap extract (duplicate w) === w
      coassociativity :: w a -> Property
      coassociativity w =
        duplicate (duplicate w) === fmap duplicate (duplicate w)
   in testGroup
        "Comonad Laws"
        [ testProperty "extract . duplicate == id" leftId
        , testProperty "fmap extract . duplicate == id" rightId
        , testProperty
            "duplicate . duplicate == fmap duplicate . duplicate"
            coassociativity
        ]

-- | @tabulate@ and @index@ are inverse, in both directions.
--
-- The suite already had @index (tabulate id) c == c@, which pins down only that
-- a coordinate survives a round trip through the layout. It says nothing about
-- the cells: a `Data.Functor.Rep.tabulate` that filled the vector in the wrong
-- order would still pass it, because the values it is compared on /are/ the
-- coordinates. @tabulate . index == id@ is the direction with the payload in it.
--
-- The second property looks weaker than the general law
-- @index (tabulate h) r == h r@ for every @h :: Rep f -> a@, and for a finite
-- representable it is not: every such @h@ is @index g@ for exactly one @g@, and
-- @g@ here is arbitrary. Quantifying over grids rather than over functions also
-- avoids needing `Function` for `SizedGrid.Coord.Coord`.
representableLaws ::
     forall f a proxy.
     ( Representable f
     , Arbitrary (f a)
     , Show (f a)
     , Eq (f a)
     , Arbitrary (Rep f)
     , Show (Rep f)
     , Arbitrary a
     , Show a
     , Eq a
     , Function a
     , CoArbitrary a
     )
  => proxy (f a)
  -> TestTree
representableLaws _ =
  let tabulateIndex :: f a -> Property
      tabulateIndex g = tabulate (index g) === g
      indexTabulate :: f a -> Rep f -> Property
      indexTabulate g r = index (tabulate (index g) :: f a) r === index g r
      -- fmap has to agree with the representation: mapping the cells and
      -- rebuilding from the mapped lookup must give the same grid. A `fmap`
      -- that reordered or dropped cells passes both laws above.
      fmapIsTabulate :: Fun a a -> f a -> Property
      fmapIsTabulate h g =
        fmap (applyFun h) g === tabulate (applyFun h . index g)
   in testGroup
        "Representable Laws"
        [ testProperty "tabulate . index == id" tabulateIndex
        , testProperty "index . tabulate == id" indexTabulate
        , testProperty "fmap h == tabulate . (h .) . index" fmapIsTabulate
        ]

-- | The `Data.Distributive.Distributive` laws.
--
-- @Grid@'s instance is @distribute = distributeRep@, so these are in one sense
-- inherited from `Representable` -- but that is the claim being tested, and it
-- is a claim about an instance that can be changed independently. The first law
-- is the one with real content for a grid: distributing a grid of grids is a
-- transpose, and doing it twice must land back where it started.
distributiveLaws ::
     forall f a proxy.
     ( Distributive f
     , Arbitrary (f a)
     , Show (f a)
     , Eq (f a)
     , Arbitrary (f (f a))
     , Show (f (f a))
     , Eq (f (f a))
     , Show (f [a])
     , Eq (f [a])
     , Arbitrary a
     , Show a
     , Function a
     , CoArbitrary a
     )
  => proxy (f a)
  -> TestTree
distributiveLaws _ =
  let doubleDistribute :: f (f a) -> Property
      doubleDistribute g = distribute (distribute g) === g
      distributeIsCollectId :: [f a] -> Property
      distributeIsCollectId gs = distribute gs === collect id gs
      collectIsDistributeFmap :: Fun a (f a) -> [a] -> Property
      collectIsDistributeFmap h xs =
        collect (applyFun h) xs === distribute (fmap (applyFun h) xs)
      -- Distributing over 'Identity' can only be 'fmap Identity': there is one
      -- outer position, so nothing is being combined.
      identityLaw :: f a -> Property
      identityLaw g = fmap runIdentity (distribute (Identity g)) === g
   in testGroup
        "Distributive Laws"
        [ testProperty "distribute . distribute == id" doubleDistribute
        , testProperty "distribute == collect id" distributeIsCollectId
        , testProperty "collect f == distribute . fmap f" collectIsDistributeFmap
        , testProperty "distribute . Identity == fmap Identity" identityLaw
        ]

isCoordLaws ::
     forall c n. (IsCoord c, 1 <= n, KnownNat n)
  => Proxy (c n)
  -> TestTree
isCoordLaws _ =
  testCase "IsCoord Laws" $ do
    -- The old first law here compared 'maxCoordSize' against
    -- @natVal . sCoordSized@. Both sides are now the same expression --
    -- 'maxCoordSize' is @natVal@ of its visible argument, minus one -- so the
    -- law had become a tautology and is dropped. "Max size equality" below is
    -- the one with content: it ties the number to the value 'maxCoord'.
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

