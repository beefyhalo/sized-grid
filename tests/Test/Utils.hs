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

-- | Renders a @quickcheck-classes@ 'Laws' bundle as a 'TestTree', the same
-- way 'testGroup' renders any other named list of properties. This is what
-- lets our own law bundles below sit in the same list as upstream's
-- 'Test.QuickCheck.Classes.functorLaws' and friends and come out
-- indistinguishable in the test-tree output.
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
--
-- Shaped as a @quickcheck-classes@ 'Laws' bundle (sized-grid-05b), the same
-- shape 'Test.QuickCheck.Classes.functorLaws' and friends use: a 'Proxy w'
-- with the element type fixed internally, rather than a 'Proxy (w a)'. That
-- drops a type argument callers no longer need to supply, and it is what lets
-- 'lawsToTest' render this alongside upstream's bundles with nothing in the
-- output to tell them apart. The element type is fixed to 'Int', matching the
-- 'Int' payload used at every other call site in this suite (upstream fixes
-- it to 'Integer'). The quantified constraints below discharge from the
-- concrete instances at a fixed @cs@ with no new machinery -- even
-- @Show (w (w (w a)))@, three levels deep for coassociativity, resolves
-- because each nesting is the same quantified constraint applied one level
-- further in.
comonadLaws ::
     forall w.
     ( Comonad w
     , forall a. Arbitrary a => Arbitrary (w a)
     , forall a. Show a => Show (w a)
     , forall a. Eq a => Eq (w a)
     )
  => Proxy w
  -> Laws
comonadLaws _ =
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
     forall f.
     ( Representable f
     , forall a. Arbitrary a => Arbitrary (f a)
     , forall a. Show a => Show (f a)
     , forall a. Eq a => Eq (f a)
     , Arbitrary (Rep f)
     , Show (Rep f)
     )
  => Proxy f
  -> Laws
representableLaws _ =
  let tabulateIndex :: f Int -> Property
      tabulateIndex g = tabulate (index g) === g
      indexTabulate :: f Int -> Rep f -> Property
      indexTabulate g r = index (tabulate (index g) :: f Int) r === index g r
      -- fmap has to agree with the representation: mapping the cells and
      -- rebuilding from the mapped lookup must give the same grid. A `fmap`
      -- that reordered or dropped cells passes both laws above.
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
--
-- @Grid@'s instance is @distribute = distributeRep@, so these are in one sense
-- inherited from `Representable` -- but that is the claim being tested, and it
-- is a claim about an instance that can be changed independently. The first law
-- is the one with real content for a grid: distributing a grid of grids is a
-- transpose, and doing it twice must land back where it started.
distributiveLaws ::
     forall f.
     ( Distributive f
     , forall a. Arbitrary a => Arbitrary (f a)
     , forall a. Show a => Show (f a)
     , forall a. Eq a => Eq (f a)
     )
  => Proxy f
  -> Laws
distributiveLaws _ =
  let doubleDistribute :: f (f Int) -> Property
      doubleDistribute g = distribute (distribute g) === g
      distributeIsCollectId :: [f Int] -> Property
      distributeIsCollectId gs = distribute gs === collect id gs
      collectIsDistributeFmap :: Fun Int (f Int) -> [Int] -> Property
      collectIsDistributeFmap h xs =
        collect (applyFun h) xs === distribute (fmap (applyFun h) xs)
      -- Distributing over 'Identity' can only be 'fmap Identity': there is one
      -- outer position, so nothing is being combined.
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

