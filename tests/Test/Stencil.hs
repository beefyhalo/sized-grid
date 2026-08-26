{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Tests for the precomputed neighbourhood (@sized-grid-adr.2@).
--
-- The whole claim of "Data.Grid.Sized.Stencil" is that flattening a
-- neighbourhood to vector positions changes nothing but the cost. So every
-- property here is stated against the loop it replaces --- @imapGrid@ over
-- @mooreNeighbours@ --- rather than against a hand-written expectation, and the
-- axis lists are mixed on purpose: a `Clamped` axis drops neighbours at its
-- edge, a `Periodic` one never does, and a `Reflective` one reaches the same
-- cell from two offsets and so is deduplicated. Those three behaviours are what
-- a table of positions could silently flatten away.
module Test.Stencil
  ( stencilTests
  ) where

import           Data.Grid.Sized
import           Data.Grid.Sized.Unboxed (UGrid)
import           Test.Arbitrary          ()

import qualified Data.Vector.Generic     as VG
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck   (testProperty, (===))

-- | @stencilGrid@ against the loop it exists to replace, at a given radius.
--
-- @f@ is @(,)@ rather than a fold, so the neighbours are compared as a list:
-- their order and their multiplicity are part of the claim, and a sum would
-- hide both.
agreesWithNeighbours ::
       forall cs.
       (IsCoordList cs, AllSizedKnown cs)
    => String
    -> Int
    -> TestTree
agreesWithNeighbours name r =
    testProperty name $ \(g :: Grid cs Int) ->
        gridVector (stencilGrid (mooreStencil r) (,) g) ===
        gridVector
            (imapGrid
                 (\c x -> (x, map (indexGrid g) (mooreNeighbours r c)))
                 g)

-- | The same for the von Neumann neighbourhood, which differs from Moore in
-- which entries survive rather than in how they are laid out --- so it is the
-- check that 'stencilFor' does not quietly assume a full product.
agreesWithVonNeumann ::
       forall cs.
       (IsCoordList cs, AllSizedKnown cs)
    => String
    -> Int
    -> TestTree
agreesWithVonNeumann name r =
    testProperty name $ \(g :: Grid cs Int) ->
        gridVector (stencilGrid (vonNeumannStencil r) (,) g) ===
        gridVector
            (imapGrid
                 (\c x -> (x, map (indexGrid g) (vonNeumannNeighbours r c)))
                 g)

-- | 'stencilFoldGrid' against 'stencilGrid' composed with a strict left fold
-- (@sized-grid-adr.13@): the whole claim of the fold-shaped variant is that it
-- computes the same thing 'stencilGrid' does without building the list, so it
-- is checked against 'stencilGrid' rather than against the neighbourhood loop
-- a second time.
foldGridAgreesWithStencilGrid ::
       forall cs.
       (IsCoordList cs, AllSizedKnown cs)
    => String
    -> Int
    -> TestTree
foldGridAgreesWithStencilGrid name r =
    testProperty name $ \(g :: Grid cs Int) ->
        gridVector (stencilFoldGrid (mooreStencil r) (+) id g) ===
        gridVector (stencilGrid (mooreStencil r) (foldl' (+)) g)

-- | 'stencilAt' against the same loop, one cell at a time.
--
-- Stated separately from 'agreesWithNeighbours' rather than derived from it:
-- 'stencilGrid' walks the table by vector position and this walks it by
-- coordinate, so only this one exercises the @coordPosition@ that picks the
-- row, and a layout that transposed the table would still pass the other.
readsOneCellLikeTheLoop ::
       forall cs. (IsCoordList cs, AllSizedKnown cs)
    => String
    -> Int
    -> TestTree
readsOneCellLikeTheLoop name r =
    testProperty name $ \(g :: Grid cs Int) ->
        let s = mooreStencil r
         in map (stencilAt s g) (allCoord @cs) ===
            map (map (indexGrid g) . mooreNeighbours r) (allCoord @cs)

-- | The width a stencil discovers, against the widest row @mooreNeighbours@
-- actually produces. Separate from the agreement properties because those
-- would still pass if the width were too large: an over-wide table pads with
-- sentinels the reader stops at, so it costs memory and a little time and
-- changes no answer.
widthIsTheWidestRow ::
       forall cs. (IsCoordList cs)
    => String
    -> Int
    -> TestTree
widthIsTheWidestRow name r =
    testCase name $
    assertEqual
        ""
        (maximum (0 : [length (mooreNeighbours r c) | c <- allCoord @cs]))
        (stencilWidth (mooreStencil @cs r))

-- | @mooreStencil@ against @stencilFor (mooreNeighbours r)@ on the same axis
-- list and radius: not just agreement with the neighbourhood loop --
-- 'agreesWithNeighbours' above already states that -- but exact equality
-- with the two-pass table 'mooreStencil' used to be, width and positions
-- both. That is the property @sized-grid-fup0@ turns on: the bounded builder
-- has to produce the identical table the discovered-width one does, not
-- merely one that reads the same through 'stencilGrid'.
mooreAgreesWithStencilFor ::
       forall cs. IsCoordList cs
    => String
    -> Int
    -> TestTree
mooreAgreesWithStencilFor name r =
    testCase name $ do
        let bounded = mooreStencil @cs r
            general = stencilFor @cs (mooreNeighbours r)
        assertEqual "width" (stencilWidth general) (stencilWidth bounded)
        assertEqual
            "positions"
            (VG.toList (stencilPositions general))
            (VG.toList (stencilPositions bounded))

-- | The same for 'vonNeumannStencil', whose allocation bound
-- ('Data.Grid.Sized.Stencil.mooreUpperBound') is the loose one --
-- @sized-grid-fup0@'s compacting branch is only exercised by this one.
vonNeumannAgreesWithStencilFor ::
       forall cs. IsCoordList cs
    => String
    -> Int
    -> TestTree
vonNeumannAgreesWithStencilFor name r =
    testCase name $ do
        let bounded = vonNeumannStencil @cs r
            general = stencilFor @cs (vonNeumannNeighbours r)
        assertEqual "width" (stencilWidth general) (stencilWidth bounded)
        assertEqual
            "positions"
            (VG.toList (stencilPositions general))
            (VG.toList (stencilPositions bounded))

stencilTests :: TestTree
stencilTests =
    testGroup
        "Stencil agrees with the neighbourhood it precomputes"
        [ testGroup
              "Moore, radius 1"
              [ agreesWithNeighbours @'[ Clamped 5, Clamped 4] "bounded" 1
              , agreesWithNeighbours @'[ Periodic 5, Periodic 4] "torus" 1
              , agreesWithNeighbours @'[ Periodic 5, Clamped 4] "cylinder" 1
              , agreesWithNeighbours @'[ Clamped 5, Reflective 4] "bounded x reflecting" 1
              , agreesWithNeighbours @'[ Reflect101 4, Periodic 3] "reflect101 x torus" 1
              , agreesWithNeighbours @'[ Clamped 4, Clamped 3, Periodic 2] "three axes" 1
              , agreesWithNeighbours @'[ Clamped 4] "one axis" 1
              ]
        , testGroup
              "Moore, radius 2 --- wide enough to reach past a small axis"
              [ agreesWithNeighbours @'[ Clamped 3, Clamped 3] "bounded" 2
              , -- A Periodic 3 axis at radius 2 reaches every one of its three
                -- values, two of them from both directions, so 'axisSteps'
                -- deduplicates. This is the case a table sized (2r+1)^d - 1
                -- would get wrong.
                agreesWithNeighbours @'[ Periodic 3, Periodic 3] "torus small enough to wrap onto itself" 2
              , agreesWithNeighbours @'[ Reflective 3, Clamped 4] "reflecting" 2
              ]
        , testGroup
              "von Neumann"
              [ agreesWithVonNeumann @'[ Clamped 5, Clamped 4] "bounded, radius 1" 1
              , agreesWithVonNeumann @'[ Periodic 5, Periodic 4] "torus, radius 1" 1
              , agreesWithVonNeumann @'[ Clamped 4, Periodic 4] "mixed, radius 2" 2
              ]
        , testGroup
              "stencilFoldGrid agrees with stencilGrid (sized-grid-adr.13)"
              [ foldGridAgreesWithStencilGrid @'[ Clamped 5, Clamped 4] "bounded" 1
              , foldGridAgreesWithStencilGrid @'[ Periodic 5, Periodic 4] "torus" 1
              , foldGridAgreesWithStencilGrid @'[ Periodic 5, Clamped 4] "cylinder" 1
              , foldGridAgreesWithStencilGrid @'[ Reflective 3, Clamped 4] "reflecting, radius 2" 2
              , foldGridAgreesWithStencilGrid @'[ Clamped 1, Clamped 1] "one-cell grid" 1
              , foldGridAgreesWithStencilGrid @'[ Clamped 5, Clamped 5] "radius zero" 0
              ]
        , -- Same shape as "runs on an unboxed grid" above: nothing in
          -- 'stencilFoldGrid''s body distinguishes 'v', but nothing here would
          -- catch it if that drifted either.
          testCase "stencilFoldGrid runs on an unboxed grid" $
              let g :: UGrid '[ Clamped 4, Periodic 4] Int
                  g = tabulateGrid coordPosition
                  s = mooreStencil 1
               in assertEqual
                      ""
                      (VG.toList
                           (gridVector
                                (imapGrid
                                     (\c x ->
                                          x + sum (map (indexGrid g) (mooreNeighbours 1 c)))
                                     g)))
                      (VG.toList
                           (gridVector (stencilFoldGrid s (+) id g)))
        , testGroup
              "stencilAt reads one cell"
              [ readsOneCellLikeTheLoop @'[ Clamped 5, Clamped 4] "bounded" 1
              , readsOneCellLikeTheLoop @'[ Periodic 5, Periodic 4] "torus" 1
              , readsOneCellLikeTheLoop @'[ Reflective 4, Clamped 3] "reflecting x bounded" 2
              , readsOneCellLikeTheLoop @'[ Clamped 1, Clamped 1] "one-cell grid" 1
              ]
        , testGroup
              "the discovered width is the widest row"
              [ widthIsTheWidestRow @'[ Clamped 5, Clamped 4] "bounded: the interior sets it" 1
              , widthIsTheWidestRow @'[ Periodic 5, Periodic 4] "torus: every row is full" 1
              , widthIsTheWidestRow @'[ Periodic 3, Periodic 3] "deduplicated torus" 2
              ]
        , -- sized-grid-fup0: 'mooreStencil'/'vonNeumannStencil' now build
          -- through the bounded one-pass path rather than 'stencilFor'
          -- directly, so this states they still land on the exact table
          -- 'stencilFor' would have built, not merely one that agrees under
          -- 'stencilGrid'.
          testGroup
              "mooreStencil/vonNeumannStencil agree exactly with stencilFor (sized-grid-fup0)"
              [ mooreAgreesWithStencilFor @'[ Clamped 5, Periodic 4] "bounded x torus" 1
              , mooreAgreesWithStencilFor @'[ Reflective 4, Reflect101 5] "reflecting x reflect101" 2
              , -- A Periodic 3 axis at radius 2 reaches every one of its three
                -- values, two of them from both directions, so 'axisSteps'
                -- deduplicates -- the case a table sized (2r+1)^d - 1 would
                -- get wrong if the width it recorded were the bound instead
                -- of what the fill observed.
                mooreAgreesWithStencilFor @'[ Periodic 3, Periodic 3] "torus small enough to dedup" 2
              , mooreAgreesWithStencilFor @'[ Clamped 4, Clamped 3, Periodic 2] "three mixed axes" 1
              , vonNeumannAgreesWithStencilFor @'[ Clamped 4, Periodic 4] "mixed, radius 1" 1
              , -- 'mooreUpperBound' 2 2 is 24 against a von Neumann width of
                -- 12: the one case in this group where the allocation bound
                -- is loose and the compacting copy actually runs.
                vonNeumannAgreesWithStencilFor @'[ Clamped 5, Clamped 4] "bounded, loose bound" 2
              , vonNeumannAgreesWithStencilFor @'[ Periodic 3, Reflective 3] "torus x reflecting, dedup" 2
              , -- The empty neighbourhood: a zero radius names no neighbours
                -- at all, so the bound and the discovered width are both
                -- zero and the buffer 'stencilBounded' allocates is empty.
                mooreAgreesWithStencilFor @'[ Clamped 5, Clamped 5] "empty neighbourhood: radius zero" 0
              , vonNeumannAgreesWithStencilFor @'[ Clamped 5, Clamped 5] "empty neighbourhood: radius zero" 0
              , -- A grid too small for the bound to ever be tight: every row
                -- is the same short width regardless of position, so this is
                -- the compacting copy running on every row rather than the
                -- boundary rows alone.
                mooreAgreesWithStencilFor @'[ Clamped 2, Clamped 2] "every row short: tiny bounded grid" 2
              , vonNeumannAgreesWithStencilFor @'[ Clamped 2, Clamped 2] "every row short: tiny bounded grid" 2
              ]
        , testCase "a torus needs no sentinels at all" $
              assertEqual
                  ""
                  []
                  (filter (< 0) $
                   VG.toList $
                   stencilPositions (mooreStencil @'[ Periodic 5, Periodic 4] 1))
        , testCase "a bounded grid does need them" $
              assertBool "" $
              any (< 0) $
              VG.toList $
              stencilPositions (mooreStencil @'[ Clamped 5, Clamped 4] 1)
        , -- 'stencilGrid' is written at 'VG.Vector', not at the boxed grid, and
          -- the unboxed grid is the representation the automaton workloads
          -- actually use. Nothing in the body distinguishes them, but nothing
          -- would have caught it if the signature had drifted either.
          testCase "runs on an unboxed grid" $
              let g :: UGrid '[ Clamped 4, Periodic 4] Int
                  g = tabulateGrid coordPosition
                  s = mooreStencil 1
               in assertEqual
                      ""
                      (VG.toList
                           (gridVector
                                (imapGrid
                                     (\c x ->
                                          x + sum (map (indexGrid g) (mooreNeighbours 1 c)))
                                     g)))
                      (VG.toList
                           (gridVector
                                (stencilGrid s (\x ns -> x + sum ns) g)))
        , -- The degenerate shapes, which is where an off-by-one in the row
          -- arithmetic shows up rather than in the interesting cases above.
          testCase "a one-cell bounded grid has no neighbours and so a zero width" $
              assertEqual "" 0 (stencilWidth (mooreStencil @'[ Clamped 1, Clamped 1] 1))
        , testCase "a zero radius has no neighbours either" $
              assertEqual "" 0 (stencilWidth (mooreStencil @'[ Clamped 5, Clamped 5] 0))
        , agreesWithNeighbours @'[ Clamped 1, Clamped 1] "one-cell grid still round trips" 1
        , agreesWithNeighbours @'[ Clamped 5, Clamped 5] "radius zero still round trips" 0
        ]
