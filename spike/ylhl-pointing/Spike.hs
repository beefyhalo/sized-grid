{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, ScopedTypeVariables, TypeApplications,
             TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-missing-signatures #-}

-- | Spike for sized-grid-ylhl: does a /checked/ walker step exist, and does it
-- work on an @Ordinal@ axis?
--
-- Two claims, both load-bearing for the recommendation in
-- @docs\/superpowers\/specs\/2026-08-29-pointing-family-design.md@:
--
--   1. A checked step that also carries a heading needs only 'IsCoord'
--      methods -- 'offsetIsCoord' and 'axisFrameFlipsIsCoord' -- neither of
--      which mentions @Diff@. So unlike 'transportCoord' it is not excluded
--      from @Ordinal@.
--   2. With the heading indexed by @MapStep@ (one @Int@ per axis) rather than
--      @MapDiff@, a walker on an @Ordinal@-axed grid can be written down at
--      all. Today it cannot: @Diff (Coord '[Ordinal 5])@ is stuck.
--
-- Run against the library's own repl, which is what it is written to check
-- against:
--
-- > cabal repl lib:grid-sized --repl-options=-ispike/ylhl-pointing
-- > ghci> :add Spike
-- > ghci> :m + Spike
-- > ghci> ordinalWalk
-- > ghci> policyWalks
--
-- Nothing here is a proposed implementation. @posStepT@ duplicates
-- 'Data.Grid.Sized.Coord.Transform.posTransport' badly and on purpose: the
-- point is only that the obligations it needs are available, not that this is
-- how to state them.
module Spike where

import           Data.Grid.Sized
import           Data.Grid.Sized.Unsafe (unsafeGridFromVector)

import           Control.Lens           (review, view)
import qualified Data.Vector            as V
import           Generics.SOP           (NP (..))
import           GHC.TypeLits           (KnownNat)

-- | sized-grid-i0ob.2's family: one signed step count per axis, no @Diff@
-- anywhere, so it reduces on @Ordinal@.
type family MapStep cs where
  MapStep '[]       = '[]
  MapStep (x ': xs) = Int ': MapStep xs

-- | What 'Data.Grid.Sized.Coord.Transform.TransportCoordList' would be if it
-- were checked. Note the superclass: 'IsCoordList', not @AffineCoordList@.
class IsCoordList cs => CheckedTransport cs where
    posStepT :: Int -> NP I (MapStep cs) -> Maybe (Int, NP I (MapStep cs))

instance CheckedTransport '[] where
    posStepT p Nil = Just (p, Nil)

instance (IsCoordLifted x, CheckedTransport xs, IsCoordList (x ': xs)) =>
         CheckedTransport (x ': xs) where
    posStepT p (I d :* ds) =
        case p `quotRem` coordListSize @xs of
            (i, r) -> do
                let x = unsafeFromAxisIndex @x i
                x' <- offsetIsCoord @(CoordContainer x) x d
                (r', ds') <- posStepT @xs r ds
                pure
                    ( toAxisIndex x' * coordListSize @xs + r'
                    , I (if axisFrameFlipsIsCoord @(CoordContainer x) x d
                             then negate d
                             else d) :* ds')

-- | The checked counterpart of 'Data.Grid.Sized.Coord.transportCoord'.
transportCoordMaybe ::
       forall cs. CheckedTransport cs
    => Coord cs
    -> Delta (MapStep cs)
    -> Maybe (Coord cs, Delta (MapStep cs))
transportCoordMaybe c (Delta d) =
    (\(p, d') -> (unsafeCoordFromPosition p, Delta d')) <$>
    posStepT @cs (coordPosition c) d

-- | 'Data.Grid.Sized.Focused.Walker' with the heading indexed by 'MapStep'.
data W cs a = W
    { wGrid    :: FocusedGrid cs a
    , wHeading :: Delta (MapStep cs)
    }

-- | The checked counterpart of 'Data.Grid.Sized.Focused.stepWalker'.
stepWithin :: CheckedTransport cs => W cs a -> Maybe (W cs a)
stepWithin (W (FocusedGrid g p) h) =
    (\(p', h') -> W (FocusedGrid g p') h') <$> transportCoordMaybe p h

mk :: (IsCoord c, KnownNat n) => Int -> c n
mk = review asOrdinal . unsafeOrdinal

-- | Row 1, column 0, heading @(0, 1)@ -- pointing along the second axis.
atRow1 ::
       forall c n. (IsCoord c, KnownNat n, CheckedTransport '[c n, c n])
    => Grid '[c n, c n] Int
    -> W '[c n, c n] Int
atRow1 g = W (FocusedGrid g (mk 1 :| mk 0 :| EmptyCoord)) (0 :^ 1 :^ NoDelta)

-- | Claim 2: this type is inhabited. @Walker '[Ordinal 3, Ordinal 3] Int@ is
-- not -- its heading is @Delta [Diff (Ordinal 3), Diff (Ordinal 3)]@, and
-- @Diff (Ordinal 3)@ is stuck.
--
-- Expect @[3,4,5]@: the walker crosses the window and stops at the window's
-- own edge rather than wrapping to whatever the source axis was.
ordinalWalk :: [Int]
ordinalWalk = map valueAt (trail (atRow1 board))
  where
    board = unsafeGridFromVector (V.fromList [0 .. 8]) :: Grid '[Ordinal 3, Ordinal 3] Int
    valueAt (W (FocusedGrid g p) _) = indexGrid g p

trail :: CheckedTransport cs => W cs a -> [W cs a]
trail w = w : maybe [] trail (stepWithin w)

-- | Claim 1, and the thing the spike found: what a checked step means on each
-- policy. Each row is @(position, heading's second component)@, nine steps
-- deep because @Periodic@ never stops.
--
-- @
-- Ordinal   : [((1,0),1),((1,1),1),((1,2),1),((1,3),1),((1,4),1)]
-- Clamped   : [((1,0),1),((1,1),1),((1,2),1),((1,3),1),((1,4),1)]
-- Periodic  : [((1,0),1),((1,1),1),((1,2),1),((1,3),1),((1,4),1),((1,0),1),...
-- Reflective: [((1,0),1),((1,1),1),((1,2),1),((1,3),1),((1,4),1)]
-- Reflect101: [((1,0),1),((1,1),1),((1,2),1),((1,3),1),((1,4),-1),((1,3),-1),...
-- @
--
-- @Reflective@ stops at the wall and @Reflect101@ turns around, which is the
-- inconsistency the design doc reports: @Reflect101@ is the only axis where a
-- step the bounds check /accepts/ also reports a frame flip, because
-- @mirrorAt@ resolves its fixed point @r == m@ as reflected.
policyWalks :: IO ()
policyWalks = do
    report "Ordinal   " (board :: Grid '[Ordinal 5, Ordinal 5] Int)
    report "Clamped   " (board :: Grid '[Clamped 5, Clamped 5] Int)
    report "Periodic  " (board :: Grid '[Periodic 5, Periodic 5] Int)
    report "Reflective" (board :: Grid '[Reflective 5, Reflective 5] Int)
    report "Reflect101" (board :: Grid '[Reflect101 5, Reflect101 5] Int)
  where
    board :: Grid '[c 5, c 5] Int
    board = unsafeGridFromVector (V.fromList [0 .. 24])
    report name g =
        putStrLn (name ++ ": " ++ show (take 9 (map step (trail (atRow1 g)))))
    step w = (posOf w, let (_ :^ dy :^ NoDelta) = wHeading w in dy)
    posOf (W (FocusedGrid _ (a :| b :| EmptyCoord)) _) =
        (ordinalToInt (view asOrdinal a), ordinalToInt (view asOrdinal b))
