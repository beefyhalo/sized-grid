{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
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
  , isoLaws
  , isCoordLaws
  , comonadLaws
  , representableLaws
  , distributiveLaws
  ) where

import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Ordinal

-- The orphan 'Arbitrary' instances. 'isCoordLaws' takes @Arbitrary (c n)@ as a
-- constraint, because @c@ is the caller's, but it generates the @'Ordinal' n@
-- for the other direction of the iso itself and so needs that instance here.
import           Test.Arbitrary        ()

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

-- | The three comonad laws, for `Data.Grid.Sized.Focused.FocusedGrid`.
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
-- avoids needing `Function` for `Data.Grid.Sized.Coord.Coord`.
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

-- | The two round trips of an 'Control.Lens.Iso''.
--
-- Taken as an 'AnIso'' and opened with 'withIso' so that the two directions
-- arrive as ordinary functions: an 'Control.Lens.Iso'' is rank-2, and naming
-- its halves @there@ and @back@ says which round trip is which far better than
-- any pair of names for the composed properties could.
--
-- Deliberately not from @quickcheck-classes@. That package's @Laws@ bundles
-- cover the standard classes --- which is worth having, and is
-- @sized-grid-05b@ --- but it has no iso bundle, so this would be ours to write
-- either way. Keeping it dependency-free means adopting the package stays a
-- decision that issue gets to make on its own evidence, and converting this
-- function to a @Laws@ when it does is a one-liner: the shape is already
-- \"a name plus a list of named properties\".
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

-- | The laws of the coordinate abstraction itself.
--
-- 'asOrdinal' is 'IsCoord''s only required method, and its haddock states the
-- obligation outright: a coord of size @n@ /is/ an @'Ordinal' n@, seen under a
-- different boundary policy. Every other method of the class has a default
-- written in terms of it, and both `index` and `tabulate` reach a cell by going
-- through it --- so an 'Control.Lens.Iso'' whose two directions disagree does
-- not throw. It silently reads the wrong cell, everywhere, forever.
--
-- The three assertions in "distinguished values" used to be the whole of this
-- test. They are facts about 'zeroPosition' and 'maxCoord', which are single
-- values with nothing to quantify over, so they stay as they are; what was
-- missing is any exercise of the iso at an /arbitrary/ coord, in either
-- direction. That is what the properties below are.
isCoordLaws ::
     forall c n.
     ( IsCoord c
     , 1 <= n
     , KnownNat n
     , Eq (c n)
     , Show (c n)
     , Arbitrary (c n)
     )
  => Proxy (c n)
  -> TestTree
isCoordLaws _ =
  let -- The range half of the same obligation, said in numbers. Note what it
      -- does /not/ cover: it quantifies over 'arbitrary', which draws through
      -- 'Data.Grid.Sized.Ordinal.unsafeOrdinal' at an already-reduced index, so every
      -- coord it sees is in range before 'asOrdinal' is applied. A coord built
      -- by an /operation/ --- a wrong @mod@ in 'Semigroup', a wrong clamp in
      -- ('.+^') --- is not reached from here, and 'unsafeOrdinal''s own
      -- 'Control.Exception.assert' does not catch it either: cabal builds this
      -- suite at @-O1@, where assertions are compiled away.
      inRange :: c n -> Property
      inRange c =
        let i = ordinalToInt (view asOrdinal c)
         in counterexample ("position " ++ show i) $
            (0 <= i) .&&. (i < ordinalSize @n)
      -- 'reifyCoord' recovers the value as a type-level 'Nat'. Its default goes
      -- through 'asOrdinal', but 'Ordinal' overrides it with 'reifyOrdinal', so
      -- the two routes agreeing is a real obligation on an instance rather than
      -- a restatement of the default. "Test.Ordinal" checks it at every value
      -- of an @Ordinal 5@ and at one 'Data.Grid.Sized.Coord.Periodic.Periodic 5';
      -- this is the same fact quantified over the coord.
      reifyAgrees :: c n -> Property
      reifyAgrees c =
        reifyCoord c (\m -> natVal (Proxy @m)) ===
        toInteger (ordinalToInt (view asOrdinal c))
   in testGroup
        "IsCoord Laws"
        [ testCase "distinguished values" $ do
            -- The old first law here compared 'maxCoordSize' against
            -- @natVal . sCoordSized@. Both sides are now the same expression --
            -- 'maxCoordSize' is @natVal@ of its visible argument, minus one --
            -- so the law had become a tautology and is dropped. "Max size
            -- equality" below is the one with content: it ties the number to
            -- the value 'maxCoord'.
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
          -- Only one of these two round trips can fail, and the reason is worth
          -- knowing before trusting the pair: 'asOrdinal' carries no
          -- @KnownNat n@, so an instance has no way to build an @'Ordinal' n@
          -- at all --- 'unsafeOrdinal' and 'numToOrdinal' both need the size.
          -- The only ordinal it can return is the one it was handed.
          --
          -- So @view . review@ is the law written down rather than a test, and
          -- it becomes real only if 'asOrdinal' ever gains that constraint.
          -- @review . view@ is the one with teeth, and it has them because an
          -- instance can still be non-injective: an extra field dropped on the
          -- way out and invented on the way back collapses two coords onto one
          -- ordinal. Checked by mutation --- an instance of exactly that shape
          -- falsifies @review . view@ within a handful of cases while passing
          -- @view . review@ and "lands in [0, n)".
        , isoLaws "asOrdinal" (asOrdinal :: Iso' (c n) (Ordinal n))
        , testProperty "asOrdinal lands in [0, n)" inRange
        , testProperty "reifyCoord agrees with asOrdinal" reifyAgrees
        ]

