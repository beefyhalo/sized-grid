{-# LANGUAGE ExistentialQuantification #-}

-- | sized-grid-adr.8: the current 'Coord' against a flat-'Int' 'Coord', in
-- one binary, measured back to back.
--
-- Every "lib" body below is copied from @bench/Main.hs@ in grid-sized; every
-- "int" body is the same expression against "IntCoord". Pairs are adjacent so
-- that machine noise, which moves in time rather than by benchmark (see
-- @bench/README.md@), hits both sides of a ratio equally.
module Main (main) where

import           Data.Grid.Sized
import qualified IntCoord              as I
import           IntCoord              (Diffs (..))

import           Control.DeepSeq       (NFData (..))
import           Control.Exception     (evaluate)
import           Control.Lens          (ifoldl', imap)
import           Data.AffineSpace      ((.+^))
import           Data.Functor.Rep      (index, tabulate)
import           Data.Maybe            (isJust)
import           Data.Proxy            (Proxy (..))
import qualified Data.Vector           as V
import           Test.Tasty.Bench

type Big = '[ Clamped 300, Clamped 300]

type BigI = '[ I.C 300, I.C 300]

type Walk = '[ Periodic 300, Periodic 300]

type WalkI = '[ I.P 300, I.P 300]

type Step = '[ Clamped 50, Clamped 50]

type StepI = '[ I.C 50, I.C 50]

type StepP = '[ Periodic 50, Periodic 50]

type StepPI = '[ I.P 50, I.P 50]

bigGrid :: Grid Big Int
bigGrid = tabulate coordPosition

ibigGrid :: I.IGrid BigI Int
ibigGrid = I.tabulateI I.coordPosI

plainStepGrid :: Grid Step Int
plainStepGrid = tabulate coordPosition

iStepGrid :: I.IGrid StepI Int
iStepGrid = I.tabulateI I.coordPosI

plainStepGridP :: Grid StepP Int
plainStepGridP = tabulate coordPosition

iStepGridP :: I.IGrid StepPI Int
iStepGridP = I.tabulateI I.coordPosI

total :: Foldable f => f Int -> Int
total = sum

-- | The summed-area cell rule from grid-sized's own suite: a per-cell rule
-- that /destructures/ its coordinate rather than passing it along. This is
-- the side of the trade the flat representation loses --- @(:|)@ is a field
-- read on an 'NP' and a division on an 'Int'.
powerL :: Int -> Coord Big -> Int
powerL serial ((fromEnum -> y) :| (fromEnum -> x) :| _) =
    ((rack * y + serial) * rack `div` 100) `mod` 10 - 5
  where
    rack = x + 10

powerI :: Int -> I.Coord BigI -> Int
powerI serial c =
    ((rack * y + serial) * rack `div` 100) `mod` 10 - 5
  where
    (y, c') = I.coordSplitI c
    (x, _) = I.coordSplitI c'
    rack = x + 10

--------------------------------------------------------------------------------
-- Coord arithmetic. Bodies from grid-sized's bench/Main.hs.
--------------------------------------------------------------------------------

walkL :: Int -> Coord Walk -> Int
walkL 0 c = coordPosition c
walkL k c = walkL (k - 1) (c .+^ (1 :| 1 :| EmptyCoord))

walkI :: Int -> I.Coord WalkI -> Int
walkI 0 c = I.coordPosI c
walkI k c = walkI (k - 1) (I.addI c (1 :. 1 :. DEnd))

cornerReadsL :: Grid Big Int -> Int
cornerReadsL g =
    total
        [ index g (c .+^ (0 :| 0 :| EmptyCoord)) +
        index g (c .+^ (3 :| 3 :| EmptyCoord)) -
        index g (c .+^ (0 :| 3 :| EmptyCoord)) -
        index g (c .+^ (3 :| 0 :| EmptyCoord))
        | c <- allCoord @Big
        ]

cornerReadsI :: I.IGrid BigI Int -> Int
cornerReadsI g =
    total
        [ I.indexI g (I.addI c (0 :. 0 :. DEnd)) +
        I.indexI g (I.addI c (3 :. 3 :. DEnd)) -
        I.indexI g (I.addI c (0 :. 3 :. DEnd)) -
        I.indexI g (I.addI c (3 :. 0 :. DEnd))
        | c <- I.allCoordI @BigI
        ]

checkedCornerReadsFlatL :: Int -> Int
checkedCornerReadsFlatL k =
    total
        [ (if isJust (offsetCoord c (0 :| 0 :| EmptyCoord)) then 1 else 0) +
        (if isJust (offsetCoord c (k :| k :| EmptyCoord)) then 1 else 0) +
        (if isJust (offsetCoord c (0 :| k :| EmptyCoord)) then 1 else 0) +
        (if isJust (offsetCoord c (k :| 0 :| EmptyCoord)) then 1 else 0)
        | c <- allCoord @Big
        ]

checkedCornerReadsFlatI :: Int -> Int
checkedCornerReadsFlatI k =
    total
        [ (if isJust (I.offsetI c (0 :. 0 :. DEnd)) then 1 else 0) +
        (if isJust (I.offsetI c (k :. k :. DEnd)) then 1 else 0) +
        (if isJust (I.offsetI c (0 :. k :. DEnd)) then 1 else 0) +
        (if isJust (I.offsetI c (k :. 0 :. DEnd)) then 1 else 0)
        | c <- I.allCoordI @BigI
        ]

onBoundarySweepFlatL :: Int -> Int
onBoundarySweepFlatL k =
    total
        [ (if onBoundary (c .+^ (0 :| 0 :| EmptyCoord)) then 1 else 0) +
        (if onBoundary (c .+^ (k :| k :| EmptyCoord)) then 1 else 0) +
        (if onBoundary (c .+^ (0 :| k :| EmptyCoord)) then 1 else 0) +
        (if onBoundary (c .+^ (k :| 0 :| EmptyCoord)) then 1 else 0)
        | c <- allCoord @Big
        ]

onBoundarySweepFlatI :: Int -> Int
onBoundarySweepFlatI k =
    total
        [ (if I.onBoundaryI (I.addI c (0 :. 0 :. DEnd)) then 1 else 0) +
        (if I.onBoundaryI (I.addI c (k :. k :. DEnd)) then 1 else 0) +
        (if I.onBoundaryI (I.addI c (0 :. k :. DEnd)) then 1 else 0) +
        (if I.onBoundaryI (I.addI c (k :. 0 :. DEnd)) then 1 else 0)
        | c <- I.allCoordI @BigI
        ]

axisDistanceSweepFlatL :: Int -> Int
axisDistanceSweepFlatL k =
    total
        [ coordDistance c (c .+^ (0 :| 0 :| EmptyCoord)) +
        coordDistance c (c .+^ (k :| k :| EmptyCoord)) +
        coordDistance c (c .+^ (0 :| k :| EmptyCoord)) +
        coordDistance c (c .+^ (k :| 0 :| EmptyCoord))
        | c <- allCoord @Big
        ]

axisDistanceSweepFlatI :: Int -> Int
axisDistanceSweepFlatI k =
    total
        [ I.coordDistanceI c (I.addI c (0 :. 0 :. DEnd)) +
        I.coordDistanceI c (I.addI c (k :. k :. DEnd)) +
        I.coordDistanceI c (I.addI c (0 :. k :. DEnd)) +
        I.coordDistanceI c (I.addI c (k :. 0 :. DEnd))
        | c <- I.allCoordI @BigI
        ]

--------------------------------------------------------------------------------
-- The neighbourhood loop the stencil replaces.
--------------------------------------------------------------------------------

neighbourStepL :: IsCoordList cs => Grid cs Int -> Grid cs Int
neighbourStepL g = imapGrid (\c _ -> total (map (indexGrid g) (neighbours c))) g

neighbourStepI :: I.Shape cs => I.IGrid cs Int -> I.IGrid cs Int
neighbourStepI g = I.imapI (\c _ -> total (map (I.indexI g) (I.neighboursI c))) g

--------------------------------------------------------------------------------
-- The non-constant-folded call site the issue asks about: the axis list
-- arrives as an existential dictionary, so GHC cannot see the sizes, cannot
-- unroll the fold, and cannot turn a division by the stride into a
-- multiply-shift.
--------------------------------------------------------------------------------

data Opaque2L =
    forall a b. (IsCoordList '[ a, b], AllDiffSame Int '[ a, b]) =>
                Opaque2L (Proxy '[ a, b])

data Opaque2I =
    forall a b. I.Shape '[ a, b] =>
                Opaque2I (Proxy '[ a, b])

opaqueBigL :: Opaque2L
opaqueBigL = Opaque2L (Proxy @Big)

{-# NOINLINE opaqueBigL #-}

opaqueBigI :: Opaque2I
opaqueBigI = Opaque2I (Proxy @BigI)

{-# NOINLINE opaqueBigI #-}

opaqueSweepL :: Int -> Opaque2L -> Int
opaqueSweepL k (Opaque2L (_ :: Proxy '[ a, b])) =
    total
        [ (if isJust (offsetCoord c (k :| k :| EmptyCoord)) then 1 else 0) +
        (if onBoundary c then 1 else 0) + coordDistance c zeroCoord
        | c <- allCoord @'[ a, b]
        ]

opaqueSweepI :: Int -> Opaque2I -> Int
opaqueSweepI k (Opaque2I (_ :: Proxy '[ a, b])) =
    total
        [ (if isJust (I.offsetI c (k :. k :. DEnd)) then 1 else 0) +
        (if I.onBoundaryI c then 1 else 0) + I.coordDistanceI c I.zeroCoordI
        | c <- I.allCoordI @'[ a, b]
        ]

--------------------------------------------------------------------------------
-- Agreement check: numbers from two implementations that compute different
-- things are worthless.
--------------------------------------------------------------------------------

check :: String -> Bool -> IO ()
check name ok
    | ok = pure ()
    | otherwise = error ("adr.8 spike disagrees with grid-sized on: " ++ name)

agreement :: IO ()
agreement = do
    check "allCoord positions" $
        [coordPosition c | c <- allCoord @Step] ==
        [I.coordPosI c | c <- I.allCoordI @StepI]
    check "tabulate" $
        [index plainStepGrid c | c <- allCoord @Step] ==
        [I.indexI iStepGrid c | c <- I.allCoordI @StepI]
    check "neighbours, Clamped" $
        [ map coordPosition (neighbours c) | c <- allCoord @Step] ==
        [ map I.coordPosI (I.neighboursI c) | c <- I.allCoordI @StepI]
    check "neighbours, Periodic" $
        [ map coordPosition (neighbours c) | c <- allCoord @StepP] ==
        [ map I.coordPosI (I.neighboursI c) | c <- I.allCoordI @StepPI]
    check "neighbourStep, Clamped" $
        concat (collapseGrid (neighbourStepL plainStepGrid)) ==
        V.toList (I.igridVector (neighbourStepI iStepGrid))
    check "neighbourStep, Periodic" $
        concat (collapseGrid (neighbourStepL plainStepGridP)) ==
        V.toList (I.igridVector (neighbourStepI iStepGridP))
    check "(.+^), Clamped" $
        [ coordPosition (c .+^ (3 :| (-2) :| EmptyCoord)) | c <- allCoord @Step] ==
        [ I.coordPosI (I.addI c (3 :. (-2) :. DEnd)) | c <- I.allCoordI @StepI]
    check "(.+^), Periodic" $
        [ coordPosition (c .+^ (3 :| (-2) :| EmptyCoord)) | c <- allCoord @StepP] ==
        [ I.coordPosI (I.addI c (3 :. (-2) :. DEnd)) | c <- I.allCoordI @StepPI]
    check "offsetCoord, Clamped" $
        [ fmap coordPosition (offsetCoord c (3 :| (-2) :| EmptyCoord))
        | c <- allCoord @Step
        ] ==
        [ fmap I.coordPosI (I.offsetI c (3 :. (-2) :. DEnd))
        | c <- I.allCoordI @StepI
        ]
    check "onBoundary" $
        [onBoundary c | c <- allCoord @Step] ==
        [I.onBoundaryI c | c <- I.allCoordI @StepI]
    check "coordDistance, Clamped" $
        [coordDistance c zeroCoord | c <- allCoord @Step] ==
        [I.coordDistanceI c I.zeroCoordI | c <- I.allCoordI @StepI]
    check "coordDistance, Periodic" $
        [coordDistance c zeroCoord | c <- allCoord @StepP] ==
        [I.coordDistanceI c I.zeroCoordI | c <- I.allCoordI @StepPI]
    check "transpose" $
        [ index (transposeGrid plainStepGrid) c | c <- allCoord @Step] ==
        [ I.indexI (I.transposeI iStepGrid) c | c <- I.allCoordI @StepI]
    check "the four sweeps" $
        and
            [ cornerReadsL bigGrid == cornerReadsI ibigGrid
            , checkedCornerReadsFlatL 3 == checkedCornerReadsFlatI 3
            , onBoundarySweepFlatL 3 == onBoundarySweepFlatI 3
            , axisDistanceSweepFlatL 3 == axisDistanceSweepFlatI 3
            , walkL 10000 zeroCoord == walkI 10000 I.zeroCoordI
            , map (powerL 18) (allCoord @Big) ==
              map (powerI 18) (I.allCoordI @BigI)
            , opaqueSweepL 3 opaqueBigL == opaqueSweepI 3 opaqueBigI
            ]

main :: IO ()
main = do
    agreement
    _ <- evaluate (rnf bigGrid)
    _ <- evaluate (rnf ibigGrid)
    _ <- evaluate (rnf plainStepGrid)
    _ <- evaluate (rnf iStepGrid)
    _ <- evaluate (rnf plainStepGridP)
    _ <- evaluate (rnf iStepGridP)
    defaultMain
        [ bgroup
              "tabulate 300x300 [coordPosition per cell]"
              [ bench "lib" $ nf (\f -> tabulate f :: Grid Big Int) coordPosition
              , bench "int" $
                nf (\f -> I.tabulateI f :: I.IGrid BigI Int) I.coordPosI
              ]
        , bgroup
              "tabulate 300x300 [rule destructures the coord]"
              [ bench "lib" $ nf (\s -> tabulate (powerL s) :: Grid Big Int) 18
              , bench "int" $
                nf (\s -> I.tabulateI (powerI s) :: I.IGrid BigI Int) 18
              ]
        , bgroup
              "index x90000, 300x300"
              [ env (pure bigGrid) $ \g ->
                    bench "lib" $ nf (\g' -> map (index g') (allCoord @Big)) g
              , env (pure ibigGrid) $ \g ->
                    bench "int" $
                    nf (\g' -> map (I.indexI g') (I.allCoordI @BigI)) g
              ]
        , bgroup
              "imap 300x300 [coordPosition per cell]"
              -- Eta-expanded on both sides, so that the mapping function is
              -- a saturated application GHC can inline into rather than a
              -- partial application it cannot. Left in the point-free form
              -- the real suite uses, the int side pays 24 B a cell for a
              -- boxed coordinate handed to an unknown function, and the lib
              -- side does not, because lens's 'imap' is a class method small
              -- enough to inline unsaturated while 'imapI' is not. That is a
              -- fact about two INLINE decisions, not about the two
              -- representations.
              [ bench "lib" $ nf (\g -> imap (\c x -> coordPosition c + x) g) bigGrid
              , bench "int" $ nf (\g -> I.imapI (\c x -> I.coordPosI c + x) g) ibigGrid
              ]
        , bgroup
              "imap 300x300, raw vector controls (no Coord at all)"
              [ env (pure (V.enumFromN (0 :: Int) 90000)) $ \v ->
                    bench "V.imap" $ nf (V.imap (\i x -> i + x)) v
              , env (pure (V.enumFromN (0 :: Int) 90000)) $ \v ->
                    bench "V.zipWith over V.generate" $
                    nf (\v' -> V.zipWith (+) (V.generate (V.length v') id) v') v
              , env (pure (V.enumFromN (0 :: Int) 90000)) $ \v ->
                    bench "V.zipWith over V.enumFromN" $
                    nf (\v' -> V.zipWith (+) (V.enumFromN 0 (V.length v')) v') v
              , env (pure (I.igridVector ibigGrid)) $ \v ->
                    bench "V.imap over the int grid's own vector" $
                    nf (V.imap (\i x -> i + x)) v
              , env (pure (I.igridVector ibigGrid)) $ \v ->
                    bench "V.imap over it, through imapI's wrapper" $
                    nf (\v' ->
                            I.igridVector
                                (I.imapI
                                     (\c x -> I.coordPosI c + x)
                                     (I.IGrid v' :: I.IGrid BigI Int)))
                        v
              ]
        , bgroup
              "ifoldl' 300x300"
              [ bench "lib" $
                whnf (ifoldl' (\c acc x -> acc + coordPosition c + x) 0) bigGrid
              , bench "int" $
                whnf (I.ifoldlI' (\c acc x -> acc + I.coordPosI c + x) 0) ibigGrid
              ]
        , bgroup
              "(.+^) x360000, four corner reads over 300x300"
              [ bench "lib" $ whnf cornerReadsL bigGrid
              , bench "int" $ whnf cornerReadsI ibigGrid
              ]
        , bgroup
              "offsetCoord x360000, checked, flat arms"
              [ bench "lib" $ whnf checkedCornerReadsFlatL 3
              , bench "int" $ whnf checkedCornerReadsFlatI 3
              ]
        , bgroup
              "onBoundary x360000, flat arms"
              [ bench "lib" $ whnf onBoundarySweepFlatL 3
              , bench "int" $ whnf onBoundarySweepFlatI 3
              ]
        , bgroup
              "coordDistance x360000, flat arms"
              [ bench "lib" $ whnf axisDistanceSweepFlatL 3
              , bench "int" $ whnf axisDistanceSweepFlatI 3
              ]
        , bgroup
              "(.+^) x10000, Periodic 300x300"
              [ bench "lib" $ whnf (`walkL` zeroCoord) 10000
              , bench "int" $ whnf (`walkI` I.zeroCoordI) 10000
              ]
        , bgroup
              "imapGrid over neighbours 50x50"
              [ bench "lib" $ nf neighbourStepL plainStepGrid
              , bench "int" $ nf neighbourStepI iStepGrid
              ]
        , bgroup
              "imapGrid over neighbours 50x50, Periodic"
              [ bench "lib" $ nf neighbourStepL plainStepGridP
              , bench "int" $ nf neighbourStepI iStepGridP
              ]
        , bgroup
              "transposeGrid 300x300 boxed"
              [ bench "lib" $ nf transposeGrid bigGrid
              , bench "int" $ nf I.transposeI ibigGrid
              ]
        , bgroup
              "opaque axis list (dictionary at run time), x90000"
              [ bench "lib" $ whnf (`opaqueSweepL` opaqueBigL) 3
              , bench "int" $ whnf (`opaqueSweepI` opaqueBigI) 3
              ]
        ]
