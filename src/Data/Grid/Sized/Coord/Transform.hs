{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Maps between coordinates: along a reflecting axis, and between axis lists
-- of different sizes.
--
-- 'transportCoord' is the one operation that has to carry a heading as well as
-- a position, because a reflecting axis turns a walker around when it bounces;
-- 'WeakenCoord' and 'StrengthenCoord' change the shape rather than the point.
module Data.Grid.Sized.Coord.Transform
  ( -- * Frame transform
    axisFrameFlips,
    transportCoord,
    TransportCoordList (..),

    -- * Accumulated frame element
    Frame,
    identityFrame,
    frameParity,
    frameReversals,
    frameFromReversals,
    FrameAfterStep (..),
    frameAfterStep,
    ThroughFrame (..),
    throughFrame,

    -- * Changing the size of a coord
    WeakenCoord (..),
    StrengthenCoord (..),
  )
where

import Data.AffineSpace
import Data.Bits (popCount, setBit, shiftL, shiftR, testBit, xor, (.|.))
import Data.Grid.Sized.Coord.Class
import Data.Grid.Sized.Coord.Delta
import Data.Grid.Sized.Coord.Internal
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits
import Generics.SOP (All, I (..), NP (..), SListI, lengthSList)

-- | A separate class from 'AffineCoordList': the fold needs both '.+^' and
-- 'Data.Grid.Sized.Coord.Class.axisFrameFlipsIsCoord' obligations at once,
-- which no single existing class states.
class (AffineCoordList cs, All IsCoordLifted cs) => TransportCoordList cs where
  posTransport ::
    (AllDiffSame Int cs) =>
    Int ->
    NP I (MapDiff cs) ->
    (Int, NP I (MapDiff cs))

instance TransportCoordList '[] where
  posTransport p Nil = (p, Nil)
  {-# INLINE posTransport #-}

instance
  (AffineSpace x, IsCoordLifted x, TransportCoordList xs) =>
  TransportCoordList (x ': xs)
  where
  -- Match the displacement, as in 'posAdd', so MapDiff reduces first.
  posTransport p (I d :* ds) =
    case p `quotRem` stride of
      (i, r) ->
        case posTransport @xs r ds of
          (r', ds') ->
            ( toAxisIndex (x .+^ d) * stride + r',
              I
                ( if axisFrameFlipsIsCoord x d
                    then negate d
                    else d
                )
                :* ds'
            )
        where
          x = unsafeFromAxisIndex @x i
    where
      stride = coordListSize @xs
  {-# INLINE posTransport #-}

-- | Whether stepping this axis by this displacement reverses its own sense of direction. 'False' for every axis type except 'Data.Grid.Sized.Coord.Reflective.Reflective' and 'Data.Grid.Sized.Coord.Reflect101.Reflect101'.
axisFrameFlips :: forall x. (IsCoordLifted x) => x -> Int -> Bool
axisFrameFlips = axisFrameFlipsIsCoord @(CoordContainer x) @(CoordNat x)

-- | Move a coordinate by a heading, and report the heading a walker facing it would have after the step.
transportCoord ::
  forall cs.
  (TransportCoordList cs, AllDiffSame Int cs) =>
  Coord cs ->
  Diff (Coord cs) ->
  (Coord cs, Diff (Coord cs))
transportCoord (Coord c) (Delta d) =
  case posTransport @cs c d of
    (c', d') -> (Coord c', Delta d')

-- | The frame a walker has accumulated along a walk: for each axis, whether
-- the walker's own sense of that axis now runs backwards against the chart's.
--
-- The group is @(Z\/2)^n@ for an @n@-axis space --- the subgroup of the
-- hypercube's symmetries that axis-aligned seams and reflecting walls can
-- generate, with no axis permuted. Composition is componentwise @xor@
-- ('Semigroup' \/ 'Monoid'), the chart's own frame is the identity, and every
-- element is its own inverse. Represented the way 'Coord' represents its own
-- group: one packed 'Int', bit @i@ for the @i@th axis (outermost is bit 0).
--
-- 'Data.Grid.Sized.Focused.walkerFrameFlips' is 'frameParity' of this --- the
-- determinant of the frame transform. The parity is enough to orient a walk on
-- a M\"obius strip or a Klein bottle, where only one axis ever mirrors; it is
-- not enough on a projective plane, where both axes mirror independently and a
-- caller reading direction keys in the walker's frame must know /which/.
--
-- The constructor is not exported; 'frameReversals' and 'frameFromReversals'
-- are the boundary.
newtype Frame (cs :: [Type]) = Frame Int
  deriving stock (Eq, Ord)

-- | The chart's own frame: nothing reversed. The identity of the group, and
-- where a walk starts.
identityFrame :: Frame cs
identityFrame = Frame 0
{-# INLINE identityFrame #-}

-- | Composing two frame transforms: componentwise @xor@.
instance Semigroup (Frame cs) where
  Frame a <> Frame b = Frame (a `xor` b)
  {-# INLINE (<>) #-}

instance Monoid (Frame cs) where
  mempty = identityFrame
  {-# INLINE mempty #-}

-- | Whether an odd number of axes are reversed --- the determinant of the
-- frame transform, and exactly the bit
-- 'Data.Grid.Sized.Focused.walkerFrameFlips' carries.
frameParity :: Frame cs -> Bool
frameParity (Frame n) = odd (popCount n)
{-# INLINE frameParity #-}

-- | The per-axis reversal bits, outermost axis first.
frameReversals :: forall cs. (SListI cs) => Frame cs -> [Bool]
frameReversals (Frame n) =
  [testBit n i | i <- [0 .. lengthSList (Proxy @cs) - 1]]

-- | Build a frame from per-axis reversal bits, outermost axis first. Bits past
-- the axis count are ignored.
frameFromReversals :: forall cs. (SListI cs) => [Bool] -> Frame cs
frameFromReversals bs =
  Frame $
    foldr
      (\(i, b) n -> if b then setBit n i else n)
      0
      (zip [0 .. lengthSList (Proxy @cs) - 1] bs)

instance (SListI cs) => Show (Frame cs) where
  showsPrec p f =
    showParen (p > 10) $
      showString "frameFromReversals " . showsPrec 11 (frameReversals f)

-- | Fold one step's per-axis reversals into a bit mask, bit @i@ for the @i@th
-- axis, keeping the axis structure instead of collapsing it to a parity.
class FrameAfterStep cs where
  frameStepMask :: (AllDiffSame Int cs) => Int -> NP I (MapDiff cs) -> Int

instance FrameAfterStep '[] where
  frameStepMask _ Nil = 0
  {-# INLINE frameStepMask #-}

instance
  (IsCoordLifted x, IsCoordList xs, FrameAfterStep xs) =>
  FrameAfterStep (x ': xs)
  where
  frameStepMask p (I d :* ds) =
    case p `quotRem` coordListSize @xs of
      (i, r) ->
        let x = unsafeFromAxisIndex @x i
            hd = if axisFrameFlips x d then 1 else 0
         in hd .|. (frameStepMask @xs r ds `shiftL` 1)
  {-# INLINE frameStepMask #-}

-- | Compose a step into an accumulated frame: for each axis, @xor@ in whether
-- this step reversed that axis's own sense (which only
-- 'Data.Grid.Sized.Coord.Reflective.Reflective' and
-- 'Data.Grid.Sized.Coord.Reflect101.Reflect101' ever do). The whole group
-- element behind the parity bit 'Data.Grid.Sized.Focused.walkerFrameFlips'
-- reports: @'Data.Grid.Sized.Focused.stepWalker'@ folds each step through this,
-- and @'Data.Grid.Sized.Focused.walkerFrameFlips' == 'frameParity' . 'Data.Grid.Sized.Focused.walkerFrame'@.
frameAfterStep ::
  forall cs.
  (FrameAfterStep cs, AllDiffSame Int cs) =>
  Coord cs ->
  Diff (Coord cs) ->
  Frame cs ->
  Frame cs
frameAfterStep (Coord c) (Delta d) (Frame f) =
  Frame (f `xor` frameStepMask @cs c d)
{-# INLINE frameAfterStep #-}

-- | Negate the displacement component on each reversed axis; the axis-list
-- fold behind 'throughFrame'.
class ThroughFrame cs where
  applyFrameNP ::
    (AllDiffSame Int cs) => Int -> NP I (MapDiff cs) -> NP I (MapDiff cs)

instance ThroughFrame '[] where
  applyFrameNP _ Nil = Nil
  {-# INLINE applyFrameNP #-}

instance (ThroughFrame xs) => ThroughFrame (x ': xs) where
  applyFrameNP m (I d :* ds) =
    I (if testBit m 0 then negate d else d) :* applyFrameNP @xs (m `shiftR` 1) ds
  {-# INLINE applyFrameNP #-}

-- | Read a heading (a displacement) through an accumulated frame: negate its
-- component on every reversed axis. Self-inverse. The 'Diff'-level analogue of
-- Sokoban's @throughTurn@ --- a player-frame view or an input reader turns a
-- key press into a chart heading with this, and back with the same call.
throughFrame ::
  forall cs.
  (ThroughFrame cs, AllDiffSame Int cs) =>
  Frame cs ->
  Diff (Coord cs) ->
  Diff (Coord cs)
throughFrame (Frame m) (Delta d) = Delta (applyFrameNP @cs m d)
{-# INLINE throughFrame #-}

class WeakenCoord as bs where
  weakenCoord :: Coord as -> Maybe (Coord bs)

instance WeakenCoord '[] '[] where
  weakenCoord = Just

instance
  ( WeakenCoord as bs,
    IsCoordLifted (c n),
    IsCoordLifted (c m),
    IsCoordList as,
    IsCoordList bs
  ) =>
  WeakenCoord (c n ': as) (c m ': bs)
  where
  weakenCoord (a :| as) = do
    bs <- weakenCoord as
    b <- weakenIsCoord a
    return (b :| bs)

class StrengthenCoord as bs where
  strengthenCoord :: Coord as -> Coord bs

instance StrengthenCoord '[] '[] where
  strengthenCoord c = c

instance
  ( StrengthenCoord as bs,
    IsCoordLifted (c n),
    IsCoordLifted (c m),
    IsCoordList as,
    IsCoordList bs,
    n <= m
  ) =>
  StrengthenCoord (c n ': as) (c m ': bs)
  where
  strengthenCoord (a :| as) = strengthenIsCoord a :| strengthenCoord as
