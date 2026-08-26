{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Taking sub-grids out of a grid: one window at a chosen offset, the
-- disjoint tiling, and every overlapping window.
--
-- The three differ in what they promise about their results, and the optics
-- follow: 'tiles' is a 'Traversal' because tiles are disjoint and cover the
-- source, while 'windows' is only a 'Fold' because windows overlap.
module Data.Grid.Sized.Internal.Grid.Windows
  ( ShrinkableGrid(..)
  , gridTiles
  , tiles
  , gridWindows
  , windows
  ) where

import           Data.Grid.Sized.Coord
import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Internal.Grid.Core
import           Data.Grid.Sized.Internal.Grid.Shape
import           Data.Grid.Sized.Internal.Type       (requiring, windowFits)

import           Control.Lens                        hiding (index)
import           Data.Constraint
import           Data.Kind                           (Type)
import           Data.Proxy                          (Proxy (..))
import qualified Data.Vector.Generic                 as VG
import           GHC.TypeLits

-- | Taking a window out of a grid, one axis at a time.
--
-- The /window arithmetic/ here is element-agnostic: it slices whole sub-grids
-- and never looks inside one. The constraint is nevertheless @'VG.Vector' v x@,
-- because the recursion goes through 'splitGrid' and 'combineGrid', which take
-- the underlying vector apart and concatenate it back.
--
-- @v@ and @x@ are kind-annotated because nothing in the signature forces them
-- to be: left implicit, GHC generalises @v@ to @k -> Type@ and the instance
-- below cannot then match it against a @Type -> Type@ vector.
class ShrinkableGrid (cs :: [Type]) (as :: [Type]) (bs :: [Type]) where
  shrinkGrid ::
       forall (v :: Type -> Type) (x :: Type). VG.Vector v x
    => Coord cs
    -> GridOf v as x
    -> GridOf v bs x

instance ShrinkableGrid '[] '[] '[] where
  shrinkGrid _ (Grid v) = Grid v

-- | @x + z <= y + 1@ says: a window of @z@ taken at any of the @x@ offsets
-- still fits inside the source of size @y@.
--
-- This was previously written @z <= x - y + 1@, which has @x@ and @y@ the wrong
-- way round. It only ever typechecked because the sole test case used
-- @x == y == 3@, where both readings collapse to @z <= 1@; the honest case of
-- windowing a 5-grid into three positions of 3 was rejected. Stating it as an
-- addition rather than a truncating subtraction also keeps it in reach of the
-- Nat solver.
--
-- @KnownNat x@ is new: 'reifyCoord' recovers the offset's type-level value by
-- comparing against the coord's size at runtime, now that an
-- 'Data.Grid.Sized.Ordinal.Ordinal' no longer carries that dictionary in every value.
--
-- The split is boxed, so the slice that picks the window is a boxed one
-- whatever @v@ the grid being shrunk uses. See 'splitGrid'.
-- @1 <= x@ and @IsCoordList cs@ are what @(':|')@ costs after
-- sized-grid-adr.16: splitting a coordinate is a division by the tail's
-- stride, so the tail's sizes have to be in scope, and the head axis has to
-- be one 'Data.Grid.Sized.Coord.Class.IsCoordLifted' can speak for. Neither
-- narrows what this instance covers --- an axis of size zero has no
-- coordinates to shrink.
instance ( KnownNat x
         , KnownNat z
         , AllSizedKnown as
         , IsCoord c
         , IsCoordList cs
         , ShrinkableGrid cs as bs
         , 1 <= x
         , x + z <= y + 1
         ) =>
         ShrinkableGrid (c x ': cs) (c y ': as) (c z ': bs) where
    shrinkGrid (c :| cs) =
        combineGrid . fmap (shrinkGrid cs) . helper . splitGrid
      where
        helper ::
             Grid '[ c y] (GridOf v as a)
          -> Grid '[ c z] (GridOf v as a)
        helper g =
            reifyCoord c $ \n ->
                withDict (windowFits @n @x @y @z) $ sliceGrid n z g


-- | Cut a grid into disjoint tiles along its outermost axis: an @Ordinal 9@
-- axis tiled by @Ordinal 3@ gives three tiles, not seven overlapping windows.
-- The tiles partition the source, so concatenating them reproduces it.
--
-- This was called @gridWindows@, which said the opposite of what it does. A
-- sliding window and a disjoint tiling are different operations; this is the
-- tiling. Sliding windows are 'gridWindows'.
--
-- @CoordNat big \`Mod\` CoordNat small ~ 0@ makes a tiling that does not divide
-- evenly a type error, so the result is always exactly
-- @CoordNat big \`Div\` CoordNat small@ tiles with no short remainder.
--
-- To tile along the /second/ axis, reach for 'zipLowerDim' and not
-- 'mapLowerDim':
--
-- > rows    = gridTiles                :: Board -> [Grid '[Ordinal 1, Ordinal 9] a]
-- > columns = zipLowerDim gridTiles    :: Board -> [Grid '[Ordinal 9, Ordinal 1] a]
-- @small@ before @v@, for the reason given on 'gridWindows'.
gridTiles :: forall small v big rest a.
               ( VG.Vector v a,
                 KnownNat (MaxCoordSize (small ': rest)),
                 CoordNat big `Mod` CoordNat small ~ 0
               )
            => GridOf v (big ': rest) a
            -> [GridOf v (small ': rest) a]
gridTiles (Grid v) =
    requiring @(CoordNat big `Mod` CoordNat small ~ 0) $
    let size = fromIntegral $ natVal (Proxy @(MaxCoordSize (small ': rest)))
    in map Grid $ splitVectorBySize size v
{-# INLINABLE gridTiles #-}

-- | 'gridTiles' as an optic. The @Mod ~ 0@ constraint that makes 'gridTiles'
-- total is exactly the statement that the tiles are disjoint and exactly
-- partition the grid, which is the precondition for a lawful 'Traversal':
-- setting through disjoint, covering foci has only one sensible meaning,
-- unlike 'gridWindows' (see 'windows'), whose foci overlap.
--
-- The write-back concatenates the (possibly modified) tiles in order, which
-- reproduces the source whenever the traversal is used as a getter -- the
-- same fact 'gridTiles'\'s own Haddock states for the read-only direction.
-- It shares its engine with 'mapLowerDim' via 'traverseChunks'.
tiles :: forall small v big rest a.
          ( VG.Vector v a
          , KnownNat (MaxCoordSize (small ': rest))
          , CoordNat big `Mod` CoordNat small ~ 0
          )
       => Traversal' (GridOf v (big ': rest) a) (GridOf v (small ': rest) a)
tiles f (Grid v) =
    requiring @(CoordNat big `Mod` CoordNat small ~ 0) $
    Grid <$>
    traverseChunks (fromIntegral $ natVal (Proxy @(MaxCoordSize (small ': rest)))) f v
{-# INLINABLE tiles #-}

-- | Every overlapping window of size @CoordNat small@ along a grid's outermost
-- axis, stride 1: an @Ordinal 9@ axis windowed by @Ordinal 3@ gives seven
-- overlapping windows -- @0..2@, @1..3@, ..., @6..8@ -- not the three disjoint
-- tiles 'gridTiles' would.
--
-- This is 'shrinkGrid' at every valid offset along the outermost axis, with the
-- other axes left untouched at each one, stated at the vector level for the
-- same reason 'gridTiles' is: 'shrinkGrid' walks its whole @Coord@ list one
-- axis at a time, which would need an identity offset invented for every axis
-- in @rest@ (an axis of size 1 in the same family, so its window equals its
-- source) purely to state "leave this alone". Nothing here needs that: a window
-- of the outer axis is a contiguous run of whole @rest@-blocks, so it is one
-- 'VG.slice', and 'gridTiles'\'s own trick of reading the block size off the
-- vector rather than the type carries over unchanged. The property that ties
-- the two readings together -- this agrees with 'shrinkGrid' at every offset --
-- is checked in "Test.Windows" rather than assumed here.
--
-- @CoordNat small <= CoordNat big@ makes a window larger than its source a type
-- error, mirroring 'gridTiles'\'s own @Mod ~ 0@: both are preconditions the
-- vector-level implementation cannot check for itself, so the type states them
-- instead.
-- @small@ is quantified before @v@, and both of these deliberately break the
-- \"vector first\" order the rest of the module uses. Nothing determines @small@
-- -- it appears only in the element type of the result list -- so the caller
-- always supplies it by type application, whereas @v@ is read off the argument.
-- The parameter that must be written comes first, so @gridWindows \@(Ordinal 3)@
-- keeps working rather than becoming @gridWindows \@_ \@(Ordinal 3)@.
gridWindows :: forall small v big rest a.
               ( VG.Vector v a
               , AllSizedKnown rest
               , KnownNat (CoordNat small)
               , CoordNat small <= CoordNat big
               )
            => GridOf v (big ': rest) a
            -> [GridOf v (small ': rest) a]
gridWindows (Grid v) =
    requiring @(CoordNat small <= CoordNat big) $
    let restSize = fromIntegral $ natVal (Proxy @(MaxCoordSize rest))
        smallSize = fromIntegral $ natVal (Proxy @(CoordNat small))
        windowSize = smallSize * restSize
        bigSize = VG.length v `div` restSize
    in [ Grid (VG.slice (off * restSize) windowSize v)
       | off <- [0 .. bigSize - smallSize]
       ]
{-# INLINABLE gridWindows #-}

-- | 'gridWindows' as an optic -- and, on purpose, no more than a 'Fold'.
--
-- A window of @Ordinal 3@ over an @Ordinal 9@ axis puts cell 2 in three
-- overlapping windows (see 'gridWindows'). A 'Traversal'\'s foci must be
-- disjoint for @'over' l f@ to have a single meaning; here it would not --
-- @over windows f@ would have to pick which of three writes to cell 2 wins,
-- and whichever it picked, the 'Traversal' law
-- @'over' l f . 'over' l g == 'over' l (f . g)@ would fail. So the only
-- lawful optic over 'gridWindows' is read-only.
--
-- Do not be tempted to write this as a 'Traversal' on the grounds that a
-- caller can be trusted to use it read-only: the failure of the law is
-- silent, wrong values rather than a type error, which is the one failure
-- mode this library organises its types against.
windows :: forall small v big rest a.
           ( VG.Vector v a
           , AllSizedKnown rest
           , KnownNat (CoordNat small)
           , CoordNat small <= CoordNat big
           )
        => Fold (GridOf v (big ': rest) a) (GridOf v (small ': rest) a)
windows = folding (gridWindows @small)
{-# INLINABLE windows #-}
