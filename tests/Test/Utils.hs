{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Utils
  ( eq1Laws,
    aesonLaws,
    jsonKeyLaws,
    semigroupLaws,
    monoidLaws,
    groupLaws,
    abelianLaws,
    additiveGroupLaws,
    pseudoAffineLaws,
    interiorActionLaws,
    coordRangeLaws,
    enumRangeLaws,
    traversalLaws,
    isoLaws,
    prismLaws,
    prismLawsFrom,
    lensLaws,
    setterLaws,
    isCoordLaws,
    zeroPositionMonoidLaws,
    zeroPositionAdditiveGroupLaws,
    comonadLaws,
    representableLaws,
    distributiveLaws,
    bindLaws,
    foldable1Laws,
    traversable1Laws,
    traversable1BranchingLaws,
    hashableLaws,
    lawsToTest,
  )
where

-- The orphan 'Arbitrary' instances: 'isCoordLaws' needs 'Ordinal''s own, not
-- just the caller's.

import Control.Comonad
-- '(<.>)' hidden: 'Data.Functor.Bind''s is wanted in 'bindLaws', not lens's
-- indexed-traversal composition of the same name.
import Control.Lens hiding (index, (<.>))
import Data.AdditiveGroup
import Data.Aeson
import Data.AffineSpace
import Data.Distributive
import Data.Foldable (fold, toList)
import Data.Foldable1 qualified as F1
import Data.Functor.Bind (Apply (..), Bind (..))
import Data.Functor.Classes
import Data.Functor.Compose
-- 'Data.Functor.Identity' is not imported: 'Data.Functor.Rep' re-exports it.
import Data.Functor.Rep
import Data.Grid.Sized.Coord.Class
import Data.Grid.Sized.Ordinal
import Data.Group (Abelian, Group (..))
import Data.Hashable (Hashable (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Proxy
import GHC.TypeLits
import Test.Arbitrary ()
-- Hidden because the `Representable`/`Distributive` methods of the same
-- names are wanted here instead.

import Test.QuickCheck.Classes (Laws (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (collect, tabulate)

eq1Laws ::
  forall f.
  (Eq1 f, Applicative f) =>
  Proxy f ->
  TestTree
eq1Laws _ =
  let nilEq =
        assertEqual "Nil equal" True $ liftEq (==) (pure ()) (pure @f ())
   in testGroup "Eq1 Laws" [testCase "Nil Eq" nilEq]

aesonLaws ::
  forall a.
  (Show a, Eq a, ToJSON a, FromJSON a, Arbitrary a) =>
  TestTree
aesonLaws =
  let encodeDecode :: a -> Property
      encodeDecode a = Just a === decode (encode a)
   in testGroup "Aeson Laws" [testProperty "Encode decode" encodeDecode]

-- | 'ToJSONKey'\/'FromJSONKey' round-trip. Key encoding is a different code
-- path from 'ToJSON'\/'FromJSON' -- aeson only takes it via a 'Map', so that
-- is what this routes the round trip through.
jsonKeyLaws ::
  forall a.
  (Show a, Ord a, ToJSONKey a, FromJSONKey a, Arbitrary a) =>
  TestTree
jsonKeyLaws =
  let encodeDecode :: Map a Int -> Property
      encodeDecode m = Just m === decode (encode m)
   in testGroup
        "JSON Key Laws"
        [testProperty "Encode decode via Map key" encodeDecode]

semigroupLaws ::
  forall a.
  (Show a, Eq a, Semigroup a, Arbitrary a) =>
  TestTree
semigroupLaws =
  let assoc :: a -> a -> a -> Property
      assoc a b c = a <> (b <> c) === (a <> b) <> c
   in testGroup "Semigroup Laws" [testProperty "Associative" assoc]

monoidLaws ::
  forall a.
  (Show a, Eq a, Monoid a, Arbitrary a) =>
  TestTree
monoidLaws =
  let assoc :: a -> a -> a -> Property
      assoc a b c = mappend a (mappend b c) === mappend (mappend a b) c
      memptyId :: a -> Property
      memptyId a = (a === mappend mempty a) .&&. (a === mappend a mempty)
      concatIsFold :: [a] -> Property
      concatIsFold as = mconcat as === fold as
   in testGroup
        "Monoid laws"
        [ testProperty "Associative" assoc,
          testProperty "Mempty Id" memptyId,
          testProperty "Concat is Fold" concatIsFold
        ]

-- | The 'Data.Group.Group' laws: 'invert' is a two-sided inverse for '<>'.
groupLaws ::
  forall a.
  (Show a, Eq a, Group a, Arbitrary a) =>
  TestTree
groupLaws =
  let invertRight :: a -> Property
      invertRight a = a <> invert a === mempty
      invertLeft :: a -> Property
      invertLeft a = invert a <> a === mempty
   in testGroup
        "Group laws"
        [ testProperty "a <> invert a == mempty" invertRight,
          testProperty "invert a <> a == mempty" invertLeft
        ]

-- | The single 'Data.Group.Abelian' law: '<>' commutes.
abelianLaws ::
  forall a.
  (Show a, Eq a, Abelian a, Arbitrary a) =>
  TestTree
abelianLaws =
  let commute :: a -> a -> Property
      commute a b = a <> b === b <> a
   in testGroup "Abelian laws" [testProperty "a <> b == b <> a" commute]

additiveGroupLaws ::
  forall a.
  (Show a, Eq a, AdditiveGroup a, Arbitrary a) =>
  TestTree
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
        [ testProperty "Associative" assoc,
          testProperty "Zero Id" zeroId,
          testProperty "Inverse id is zeroV" inverseId,
          testProperty "a - (a - b) = b" takeLeaves
        ]

-- | The PseudoAffine laws shared by the coordinate policies. Full
-- 'AffineSpace' associativity is intentionally omitted because a bounded
-- policy can retract a step at its wall.
pseudoAffineLaws ::
  forall a.
  (Arbitrary a, Show a, Eq a, AffineSpace a, Eq (Diff a), Show (Diff a)) =>
  TestTree
pseudoAffineLaws =
  let addZero :: a -> Property
      addZero a = a === a .+^ zeroV
      takeSelf :: a -> Property
      takeSelf a = a .-. a === zeroV
      -- The law that pins down (.-.): a difference must carry you back.
      subtractThenAdd :: a -> a -> Property
      subtractThenAdd a b = a === b .+^ (a .-. b)
   in testGroup
        "PseudoAffine Laws"
        -- Associativity is intentionally absent: it fails at the walls for
        -- retracting actions, although it holds wherever every leg stays
        -- inside the space. 'interiorActionLaws' records that restricted law.
        [ testProperty "Add Zero" addZero,
          testProperty "Take self" takeSelf,
          testProperty "b .+^ (a .-. b) == a" subtractThenAdd
        ]

interiorActionLaws ::
  forall c n.
  ( IsCoord c,
    KnownNat n,
    1 <= n,
    Arbitrary (c n),
    Show (c n),
    Eq (c n),
    AffineSpace (c n),
    Diff (c n) ~ Int
  ) =>
  TestTree
interiorActionLaws =
  let associativity :: c n -> Int -> Int -> Property
      associativity p u v =
        case offsetIsCoord p u of
          Nothing -> property True
          Just pu ->
            case (offsetIsCoord pu v, offsetIsCoord p (u + v)) of
              (Just _, Just fused) -> pu .+^ v === fused
              _ -> property True
   in testGroup
        "Interior AffineSpace Laws"
        [testProperty "Associative where every leg stays inside" associativity]

coordRangeLaws ::
  forall c n.
  ( IsCoord c,
    KnownNat n,
    Arbitrary (c n),
    Show (c n),
    Semigroup (c n),
    AffineSpace (c n),
    Diff (c n) ~ Int
  ) =>
  TestTree
coordRangeLaws =
  let inRange :: c n -> Property
      inRange c =
        let i = ordinalToInt (view asOrdinal c)
         in counterexample ("position " ++ show i) $
              (0 <= i) .&&. (i < ordinalSize @n)
      combine :: c n -> c n -> Property
      combine a b = inRange (a <> b)
      offset :: c n -> Int -> Property
      offset c d = inRange (c .+^ d)
   in testGroup
        "Coordinate operation range"
        [ testProperty "Semigroup stays in range" combine,
          testProperty "Affine displacement stays in range" offset
        ]

enumRangeLaws ::
  forall c n.
  ( IsCoord c,
    KnownNat n,
    Enum (c n)
  ) =>
  TestTree
enumRangeLaws =
  testGroup
    "Enum operation range"
    [ testProperty "toEnum stays in range" $ \i ->
        let c = toEnum i :: c n
            position = ordinalToInt (view asOrdinal c)
         in counterexample ("position " ++ show position) $
              (0 <= position) .&&. (position < ordinalSize @n)
    ]

-- | Renders a @quickcheck-classes@ 'Laws' bundle as a 'TestTree'.
lawsToTest :: Laws -> TestTree
lawsToTest (Laws name props) = testGroup name (map (uncurry testProperty) props)

traversalLaws ::
  forall a f b.
  ( Eq a,
    Show a,
    Functor f,
    Arbitrary a,
    Function b,
    CoArbitrary b,
    Arbitrary b
  ) =>
  Traversal' a (f b) ->
  TestTree
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
            ( fmap (t (raiseFunc fFunc)) (t (raiseFunc gFunc) a)
                === getCompose
                  (t (Compose . fmap (raiseFunc fFunc) . raiseFunc gFunc) a)
            )
   in testGroup
        "Traveral Laws"
        [testProperty "Pure Id" pureId, testProperty "Compose" compose]

-- | The three comonad laws for `Data.Grid.Sized.Focused.FocusedGrid`.
--
-- Coassociativity builds a grid of grids of grids, so @w@ should stay small.
comonadLaws ::
  forall w.
  ( Comonad w,
    forall a. (Arbitrary a) => Arbitrary (w a),
    forall a. (Show a) => Show (w a),
    forall a. (Eq a) => Eq (w a)
  ) =>
  Laws
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
        [ ("extract . duplicate == id", property leftId),
          ("fmap extract . duplicate == id", property rightId),
          ( "duplicate . duplicate == fmap duplicate . duplicate",
            property coassociativity
          )
        ]

-- | The `Hashable` law that has teeth: equal values hash equally, at every
-- salt. Hand-rolled -- @quickcheck-classes@ ships no bundle for it -- and
-- stated over a structurally rebuilt copy rather than the raw input, so the
-- "equal" side is a value distinct from @x@ that @('==')@ still accepts, not a
-- reflexivity no-op. Checked for both @hashWithSalt@ (at an arbitrary salt)
-- and the salt-free @hash@.
hashableLaws ::
  forall f.
  ( Functor f,
    Hashable (f Int),
    Show (f Int),
    Arbitrary (f Int)
  ) =>
  Proxy (f Int) ->
  Laws
hashableLaws _ =
  let -- A value distinct from @x@ that @('==')@ still accepts, so the
      -- congruence property below is not a disguised reflexivity check.
      rebuilt :: f Int -> f Int
      rebuilt = fmap (\v -> v + 1 - 1)
      saltedCongruence :: Int -> f Int -> Property
      saltedCongruence salt x =
        (x === rebuilt x)
          .&&. (hashWithSalt salt x === hashWithSalt salt (rebuilt x))
      hashCongruence :: f Int -> Property
      hashCongruence x = hash x === hash (rebuilt x)
   in Laws
        "Hashable"
        [ ("x == y => hashWithSalt s x == hashWithSalt s y", property saltedCongruence),
          ("x == y => hash x == hash y", property hashCongruence)
        ]

-- | @tabulate@ and @index@ are inverse, in both directions.
representableLaws ::
  forall f.
  ( Representable f,
    forall a. (Arbitrary a) => Arbitrary (f a),
    forall a. (Show a) => Show (f a),
    forall a. (Eq a) => Eq (f a),
    Arbitrary (Rep f),
    Show (Rep f)
  ) =>
  Laws
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
        [ ("tabulate . index == id", property tabulateIndex),
          ("index . tabulate == id", property indexTabulate),
          ("fmap h == tabulate . (h .) . index", property fmapIsTabulate)
        ]

-- | The `Data.Distributive.Distributive` laws.
distributiveLaws ::
  forall f.
  ( Distributive f,
    forall a. (Arbitrary a) => Arbitrary (f a),
    forall a. (Show a) => Show (f a),
    forall a. (Eq a) => Eq (f a)
  ) =>
  Laws
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
        [ ("distribute . distribute == id", property doubleDistribute),
          ("distribute == collect id", property distributeIsCollectId),
          ("collect f == distribute . fmap f", property collectIsDistributeFmap),
          ("distribute . Identity == fmap Identity", property identityLaw)
        ]

-- | The 'Data.Functor.Bind.Bind' laws. 'quickcheck-classes' checks 'Apply'
-- ('applyLaws'), but has no equivalent for 'Bind', so this is hand-rolled the
-- way 'comonadLaws' and 'representableLaws' above are.
--
-- Associativity is the substantive law; the other two are really checks that
-- 'Apply' and 'Bind' agree, which sized-grid-o9s made a real question by
-- giving 'Grid' independent instances of both rather than deriving one from
-- the other.
bindLaws ::
  forall f.
  ( Bind f,
    forall a. (Arbitrary a) => Arbitrary (f a),
    forall a. (Show a) => Show (f a),
    forall a. (Eq a) => Eq (f a)
  ) =>
  Laws
bindLaws =
  let associativity ::
        f Int -> Fun Int (f Int) -> Fun Int (f Int) -> Property
      associativity m (applyFun -> f) (applyFun -> g) =
        ((m >>- f) >>- g) === (m >>- (\x -> f x >>- g))
      joinIsBindId :: f (f Int) -> Property
      joinIsBindId m = join m === (m >>- id)
      apIsBind :: f (Fun Int Int) -> f Int -> Property
      apIsBind (fmap applyFun -> fs) xs = (fs <.> xs) === (fs >>- (<$> xs))
   in Laws
        "Bind"
        [ ("Associativity", property associativity),
          ("join == (>>- id)", property joinIsBindId),
          ("(<.>) == (>>- (<$>))", property apIsBind)
        ]

-- | 'Data.Foldable1.Foldable1' has no 'quickcheck-classes' bundle, so this
-- pins it to the value's ordinary 'Foldable': on a structure that is never
-- empty the non-empty folds must agree element for element with the plain
-- ones, and the strict variants must agree with the lazy ones. Hand-rolled
-- the way 'bindLaws' and 'representableLaws' above are.
foldable1Laws ::
  forall f.
  ( F1.Foldable1 f,
    forall a. (Arbitrary a) => Arbitrary (f a),
    forall a. (Show a) => Show (f a)
  ) =>
  Laws
foldable1Laws =
  let toNonEmptyIsToList :: f Int -> Property
      toNonEmptyIsToList g = NE.toList (F1.toNonEmpty g) === toList g
      -- Map into a non-unital use of the list semigroup: '(0 :)' keeps every
      -- image non-empty, so 'foldMap1' (no 'mempty') and 'foldMap' agree.
      foldMap1IsFoldMap :: Fun Int [Int] -> f Int -> Property
      foldMap1IsFoldMap (applyFun -> h) g =
        F1.foldMap1 (NE.fromList . (0 :) . h) g
          === NE.fromList (foldMap ((0 :) . h) g)
      foldMap1'IsFoldMap1 :: Fun Int [Int] -> f Int -> Property
      foldMap1'IsFoldMap1 (applyFun -> h) g =
        F1.foldMap1' (NE.fromList . (0 :) . h) g
          === F1.foldMap1 (NE.fromList . (0 :) . h) g
      headLastAgree :: f Int -> Property
      headLastAgree g =
        let xs = NE.fromList (toList g)
         in (F1.head g, F1.last g) === (NE.head xs, NE.last xs)
      maxMinAgree :: f Int -> Property
      maxMinAgree g =
        let xs = NE.fromList (toList g)
         in (F1.maximum g, F1.minimum g) === (maximum xs, minimum xs)
      foldrMap1RebuildsToList :: f Int -> Property
      foldrMap1RebuildsToList g = F1.foldrMap1 pure (:) g === toList g
      foldlMap1'RebuildsToList :: f Int -> Property
      foldlMap1'RebuildsToList g =
        F1.foldlMap1' pure (\acc x -> acc ++ [x]) g === toList g
   in Laws
        "Foldable1"
        [ ("toNonEmpty == fromList . toList", property toNonEmptyIsToList),
          ("foldMap1 agrees with foldMap", property foldMap1IsFoldMap),
          ("foldMap1' agrees with foldMap1", property foldMap1'IsFoldMap1),
          ("head/last agree with toList", property headLastAgree),
          ("maximum/minimum agree with toList", property maxMinAgree),
          ("foldrMap1 (:) rebuilds toList", property foldrMap1RebuildsToList),
          ("foldlMap1' rebuilds toList", property foldlMap1'RebuildsToList)
        ]

-- | 'Traversable1' likewise has no 'quickcheck-classes' bundle. 'traverse1'
-- must agree with 'traverse' on any 'Applicative' -- checked here against
-- 'Identity' and 'Maybe' -- and 'traverse1' with 'Identity' is the identity.
--
-- Both of these cost one traversal per test, so this bundle can be
-- instantiated at any size. The branching case, which cannot, is
-- 'traversable1BranchingLaws'.
traversable1Laws ::
  forall f.
  ( Traversable1 f,
    forall a. (Arbitrary a) => Arbitrary (f a),
    forall a. (Show a) => Show (f a),
    forall a. (Eq a) => Eq (f a)
  ) =>
  Laws
traversable1Laws =
  let identityLaw :: f Int -> Property
      identityLaw g = runIdentity (traverse1 Identity g) === g
      agreesWithMaybe :: Fun Int (Maybe Int) -> f Int -> Property
      agreesWithMaybe (applyFun -> h) g = traverse1 h g === traverse h g
   in Laws
        "Traversable1"
        [ ("traverse1 Identity == Identity", property identityLaw),
          ("traverse1 agrees with traverse (Maybe)", property agreesWithMaybe)
        ]

-- | The 'Traversable1' law that cannot be stated at an arbitrary size, in its
-- own bundle so that the size restriction travels with it.
--
-- A branching 'Applicative' multiplies. Traversing @n@ cells where each
-- yields @k@ results builds @k ^ n@ structures, so at the
-- @Grid '[Periodic 10, Periodic 11]@ the rest of the suite uses -- 110 cells
-- -- even @k == 2@ is @2 ^ 110@ and the property never returns. It was
-- written that way, bundled with the two laws above and instantiated at that
-- grid, and hung the whole suite until it was killed (sized-grid-e7xo). The
-- same hazard is documented on 'Data.Grid.Sized.mapLowerDim', where
-- @mapLowerDim gridTiles@ on a 9x9 board is 387,420,489 grids.
--
-- So: instantiate this at a deliberately small structure, and read the cost
-- as @k ^ n@ before choosing one. The generated function branches two ways at
-- most, rather than into whatever length QuickCheck drew for a @Fun Int
-- [Int]@, which pins @k@ at 2 and leaves only @n@ to the caller.
--
-- It earns its keep despite that. 'Maybe' yields one result per cell, so it
-- never checks that 'traverse1' puts the @j@th result of each cell into the
-- @j@th structure; a branching applicative is what pins the rebuilt shape and
-- the order of the @('<.>')@ chain that builds it. @negate@ rather than a
-- second copy of @x@ so the two branches are told apart at @x == 0@.
traversable1BranchingLaws ::
  forall f.
  ( Traversable1 f,
    forall a. (Arbitrary a) => Arbitrary (f a),
    forall a. (Show a) => Show (f a),
    forall a. (Eq a) => Eq (f a)
  ) =>
  Laws
traversable1BranchingLaws =
  let agreesWithList :: Fun Int Bool -> f Int -> Property
      agreesWithList (applyFun -> branches) g =
        let h x
              | branches x = [x, negate x]
              | otherwise = [x]
         in traverse1 h g === traverse h g
   in Laws
        "Traversable1 (branching applicative)"
        [("traverse1 agrees with traverse ([])", property agreesWithList)]

-- | The two round trips of an 'Control.Lens.Iso''.
isoLaws ::
  (Eq s, Show s, Arbitrary s, Eq a, Show a, Arbitrary a) =>
  -- | The iso's name, used to build the test labels.
  String ->
  AnIso' s a ->
  TestTree
isoLaws name i =
  withIso i $ \there back ->
    testGroup
      (name ++ " is an isomorphism")
      [ testProperty ("review " ++ name ++ " . view " ++ name ++ " == id") $
          \s -> back (there s) === s,
        testProperty ("view " ++ name ++ " . review " ++ name ++ " == id") $
          \a -> there (back a) === a
      ]

prismLaws ::
  (Eq s, Show s, Arbitrary s, Eq a, Show a, Arbitrary a) =>
  String ->
  Prism' s a ->
  TestTree
prismLaws = prismLawsFrom arbitrary

prismLawsFrom ::
  (Eq s, Show s, Eq a, Show a, Arbitrary a) =>
  Gen s ->
  String ->
  Prism' s a ->
  TestTree
prismLawsFrom source name p =
  testGroup
    (name ++ " is a prism")
    [ testProperty ("preview " ++ name ++ " (review " ++ name ++ " a) == Just a") $
        \a -> preview p (review p a) === Just a,
      testProperty ("review " ++ name ++ " a == s when preview succeeds") $
        forAll source $ \s -> case preview p s of
          Just a -> review p a === s
          Nothing -> property True
    ]

-- | The three lens laws: get-put, put-get and put-put.
lensLaws ::
  (Eq s, Show s, Arbitrary s, Eq a, Show a, Arbitrary a) =>
  -- | The lens's name, used to build the test labels.
  String ->
  Lens' s a ->
  TestTree
lensLaws name l =
  testGroup
    (name ++ " is a lens")
    [ testProperty ("set " ++ name ++ " (view " ++ name ++ " s) s == s") $
        \s -> set l (view l s) s === s,
      testProperty ("view " ++ name ++ " (set " ++ name ++ " a s) == a") $
        \s a -> view l (set l a s) === a,
      testProperty
        ( "set "
            ++ name
            ++ " a2 (set "
            ++ name
            ++ " a1 s) == set "
            ++ name
            ++ " a2 s"
        )
        $ \s a1 a2 -> set l a2 (set l a1 s) === set l a2 s
    ]

-- | The two setter laws: 'over' with 'id' changes nothing, and two 'over's
-- compose into one.
--
-- The transforms of the focus are supplied by the caller rather than
-- generated. A 'Test.QuickCheck.Fun' would need @Function@ and @CoArbitrary@
-- at the focus type, which the focus of a grid optic -- itself a grid -- does
-- not have; two concrete transforms that do not commute with each other still
-- distinguish a setter that composes from one that does not.
setterLaws ::
  (Eq s, Show s, Arbitrary s) =>
  -- | The setter's name, used to build the test labels.
  String ->
  Setter' s a ->
  -- | A named transform of the focus.
  (String, a -> a) ->
  -- | A second one, to compose with the first.
  (String, a -> a) ->
  TestTree
setterLaws name l (fName, f) (gName, g) =
  testGroup
    (name ++ " is a setter")
    [ testProperty ("over " ++ name ++ " id == id") $ \s -> over l id s === s,
      testProperty
        ( "over "
            ++ name
            ++ " ("
            ++ fName
            ++ ") . over "
            ++ name
            ++ " ("
            ++ gName
            ++ ") == over "
            ++ name
            ++ " (("
            ++ fName
            ++ ") . ("
            ++ gName
            ++ "))"
        )
        $ \s -> over l f (over l g s) === over l (f . g) s
    ]

-- | The laws of the coordinate abstraction itself. If 'asOrdinal''s two
-- directions disagree, a coord silently reads the wrong cell, everywhere.
isCoordLaws ::
  forall c n.
  ( IsCoord c,
    1 <= n,
    KnownNat n,
    Eq (c n),
    Show (c n),
    Arbitrary (c n)
  ) =>
  TestTree
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
        reifyCoord c (\m -> natVal (Proxy @m))
          === toInteger (ordinalToInt (view asOrdinal c))
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
              (maxCoordSize n),
          -- @review . view@ is the direction with teeth: an instance can still
          -- be non-injective, dropping a field on the way out and inventing it
          -- on the way back, collapsing two coords onto one ordinal.
          isoLaws "asOrdinal" (asOrdinal :: Iso' (c n) (Ordinal n)),
          testProperty "asOrdinal lands in [0, n)" inRange,
          testProperty "reifyCoord agrees with asOrdinal" reifyAgrees
        ]

zeroPositionMonoidLaws ::
  forall c n.
  ( IsCoord c,
    1 <= n,
    KnownNat n,
    Eq (c n),
    Show (c n),
    Monoid (c n)
  ) =>
  TestTree
zeroPositionMonoidLaws =
  testProperty "zeroPosition == mempty" $
    zeroPosition @c @n === (mempty :: c n)

zeroPositionAdditiveGroupLaws ::
  forall c n.
  ( IsCoord c,
    1 <= n,
    KnownNat n,
    Eq (c n),
    Show (c n),
    AdditiveGroup (c n)
  ) =>
  TestTree
zeroPositionAdditiveGroupLaws =
  testProperty "zeroPosition == zeroV" $
    zeroPosition @c @n === (zeroV :: c n)
