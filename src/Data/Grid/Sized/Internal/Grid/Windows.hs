{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Taking sub-grids out of a grid: one window at a chosen offset, the
-- disjoint tiling, and every overlapping window.
--
-- The three differ in what they promise about their results, and the optics
-- follow: 'tiles' is a 'Traversal' because tiles are disjoint and cover the
-- source, while 'windows' is only a 'Fold' because windows overlap.
--
-- == A restriction destroys the boundary policy
--
-- Everything in this module /restricts/: it narrows a grid's extent and keeps
-- no position in the source. The rule every such operation obeys is that the
-- result is 'Data.Grid.Sized.Ordinal.Ordinal'-axed along the axis it narrowed
-- --- the policy-free axis, with no walls and no wrap, whose off-grid offsets
-- return 'Nothing' rather than an answer.
--
-- The alternative, keeping the source's axis type, is a silent wrong answer.
-- Source @[1..9]@, a window of 3 at offset 1, one step left of the window's
-- first cell:
--
-- >                            Periodic 9 -> Periodic 3   Clamped 9 -> Clamped 3
-- > source                     [1,2,3,4,5,6,7,8,9]        [1,2,3,4,5,6,7,8,9]
-- > window at offset 1         [2,3,4]                    [2,3,4]
-- > window cell 0, step left   4                          2
-- > the same step in source    1                          1
--
-- The window invents a seam that does not exist in the space it is a view of.
-- Periodicity is a property of a whole axis, and a proper sub-window of a
-- periodic axis is not periodic; \"clamped\" means stepping off the edge stays
-- at the edge, and the window's edge is not the source's edge. Both answers
-- are wrong for the same reason, and both are wrong silently, which is the
-- one failure mode this library organises its types against.
--
-- The offsets are 'Data.Grid.Sized.Ordinal.Ordinal' for the same reason read
-- from the other side. 'shrinkGrid'\'s offset into a @Periodic 9@ source
-- windowed to 3 used to have type @'Coord' \'[Periodic 7]@: the 7 is the
-- number of valid offsets, @Periodic@ is meaningless there, and @'<>'@ on
-- that coordinate wrapped offset 6 plus offset 3 round to offset 2 ---
-- arithmetic on a position in no space the caller has. An offset is an index,
-- so it is an 'Data.Grid.Sized.Ordinal.Ordinal' too.
--
-- The rule costs the full-width case a little honesty for a lot of
-- simplicity: a window whose extent equals its source\'s /does/ preserve the
-- policy, but saying so in the type needs a type-level conditional on every
-- axis. A caller who wants the policy back restates it, which is a place
-- where they have to think, which is the point.
--
-- The counterpart rule --- /a pointing preserves the boundary policy/ ---
-- belongs to "Data.Grid.Sized.Focused", which keeps the whole extent and adds
-- a position rather than the other way round. The general form of
-- \"restriction\" is
-- 'Data.Grid.Sized.Internal.Grid.Shape.permuteGrid'; the operations here
-- exist because each is one 'VG.slice' where that is a whole index table.
--
-- The same rule governs the narrowing half of the shape algebra ---
-- @takeGrid@, @dropGrid@, @sliceGrid@ and @splitHigherDim@ --- which is
-- sorted operation by operation in
-- "Data.Grid.Sized.Internal.Grid.Shape"\'s own header.
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
import           Data.Grid.Sized.Ordinal             (Ordinal)

import           Control.Lens                        hiding (index)
import           Data.Constraint
import           Data.Kind                           (Type)
import           Data.Proxy                          (Proxy (..))
import qualified Data.Vector.Generic                 as VG
import           GHC.TypeLits

-- | Taking a window out of a grid, one axis at a time.
--
-- @cs@ is the offsets, @as@ the source's axes and @bs@ the window's. Per the
-- module header, @cs@ and @bs@ are 'Data.Grid.Sized.Ordinal.Ordinal' whatever
-- @as@ is:
--
-- > shrinkGrid :: Coord '[Ordinal 3]        -- ^ which of the 3 offsets
-- >            -> Grid '[Periodic 5] a      -- ^ any source policy
-- >            -> Grid '[Ordinal 3] a       -- ^ the window has none
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

-- | The instance head says the module's rule twice over. The source axis is
-- any @c y@ --- a window can be taken out of a grid with any boundary policy
-- --- while both the /offset/ and the /window/ are
-- 'Data.Grid.Sized.Ordinal.Ordinal'. The head used to be
-- @ShrinkableGrid (c x ': cs) (c y ': as) (c z ': bs)@, which forced the
-- window's policy to equal the source's and so made a window of a
-- @Periodic 9@ wrap round its own three cells. See the module header.
--
-- @x + z <= y + 1@ says: a window of @z@ taken at any of the @x@ offsets
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
-- coordinates to shrink. @IsCoord c@ is gone with the old head: the only
-- thing the instance ever asked of the head axis was 'reifyCoord' on the
-- /offset/, and the offset is now concretely an
-- 'Data.Grid.Sized.Ordinal.Ordinal'. Nothing is asked of the source axis at
-- all, which is exactly the statement that a restriction does not care what
-- policy it is restricting.
instance ( KnownNat x
         , KnownNat z
         , AllSizedKnown as
         , IsCoordList cs
         , ShrinkableGrid cs as bs
         , 1 <= x
         , x + z <= y + 1
         ) =>
         ShrinkableGrid (Ordinal x ': cs) (c y ': as) (Ordinal z ': bs) where
    shrinkGrid (c :| cs) =
        combineGrid . fmap (shrinkGrid cs) . helper . splitGrid
      where
        helper ::
             Grid '[ c y] (GridOf v as a)
          -> Grid '[ Ordinal z] (GridOf v as a)
        helper g =
            reifyCoord c $ \n ->
                withDict (windowFits @n @x @y @z) $ sliceGrid n z g


-- | Cut a grid into disjoint tiles along its outermost axis: a source axis of
-- 9 tiled by @n ~ 3@ gives three tiles, not seven overlapping windows. The
-- tiles partition the source, so concatenating them reproduces it.
--
-- This was called @gridWindows@, which said the opposite of what it does. A
-- sliding window and a disjoint tiling are different operations; this is the
-- tiling. Sliding windows are 'gridWindows'.
--
-- @CoordNat big \`Mod\` n ~ 0@ makes a tiling that does not divide evenly a
-- type error, so the result is always exactly @CoordNat big \`Div\` n@ tiles
-- with no short remainder.
--
-- The tiled axis comes back as @'Data.Grid.Sized.Ordinal.Ordinal' n@ whatever
-- the source's was, per the module header: a tile is a proper sub-window, and
-- a proper sub-window of a periodic axis is not periodic. The remaining axes
-- are untouched at full width, so they keep their policies, which is honest
-- --- they have not been restricted. To recover a topology /across/ the tiles,
-- see the @grid-atlas@ package, which is what an atlas of charts is for.
--
-- To tile along the /second/ axis, reach for 'zipLowerDim' and not
-- 'mapLowerDim':
--
-- > rows    = gridTiles                :: Board -> [Grid '[Ordinal 1, Ordinal 9] a]
-- > columns = zipLowerDim gridTiles    :: Board -> [Grid '[Ordinal 9, Ordinal 1] a]
--
-- @n@ before @v@, for the reason given on 'gridWindows'. It is a 'Nat' rather
-- than a whole axis type because there is no longer a choice to make: the
-- result axis is 'Data.Grid.Sized.Ordinal.Ordinal', so all the caller can
-- supply is its size. @gridTiles \@(Ordinal 3)@ becomes @gridTiles \@3@.
gridTiles :: forall n v big rest a.
               ( VG.Vector v a,
                 KnownNat (MaxCoordSize (Ordinal n ': rest)),
                 CoordNat big `Mod` n ~ 0
               )
            => GridOf v (big ': rest) a
            -> [GridOf v (Ordinal n ': rest) a]
gridTiles (Grid v) =
    requiring @(CoordNat big `Mod` n ~ 0) $
    let size = fromIntegral $ natVal (Proxy @(MaxCoordSize (Ordinal n ': rest)))
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
--
-- Writing /back/ through it is where the policy rule earns its keep in the
-- other direction: the tile handed to the function is
-- 'Data.Grid.Sized.Ordinal.Ordinal'-axed, so nothing the function does to it
-- can consult a wrap or a wall the source has and the tile does not.
tiles :: forall n v big rest a.
          ( VG.Vector v a
          , KnownNat (MaxCoordSize (Ordinal n ': rest))
          , CoordNat big `Mod` n ~ 0
          )
       => Traversal' (GridOf v (big ': rest) a) (GridOf v (Ordinal n ': rest) a)
tiles f (Grid v) =
    requiring @(CoordNat big `Mod` n ~ 0) $
    Grid <$>
    traverseChunks (fromIntegral $ natVal (Proxy @(MaxCoordSize (Ordinal n ': rest)))) f v
{-# INLINABLE tiles #-}

-- | Every overlapping window of size @n@ along a grid's outermost axis,
-- stride 1: a source axis of 9 windowed by @n ~ 3@ gives seven overlapping
-- windows -- @0..2@, @1..3@, ..., @6..8@ -- not the three disjoint tiles
-- 'gridTiles' would.
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
-- is checked in "Test.Windows" rather than assumed here. That property is now
-- statable at /every/ source policy rather than only where the source is
-- already 'Data.Grid.Sized.Ordinal.Ordinal'-axed, because both sides return
-- the same 'Data.Grid.Sized.Ordinal.Ordinal'-axed window.
--
-- @n <= CoordNat big@ makes a window larger than its source a type error,
-- mirroring 'gridTiles'\'s own @Mod ~ 0@: both are preconditions the
-- vector-level implementation cannot check for itself, so the type states them
-- instead.
--
-- The windowed axis comes back as @'Data.Grid.Sized.Ordinal.Ordinal' n@
-- whatever the source's was; @rest@ is untouched at full width and keeps its
-- policies. See the module header for why. Before this, @small@ was a free
-- axis type related to @big@ only through 'CoordNat', so
-- @gridWindows \@(Clamped 3)@ over a @Grid \'[Periodic 9]@ compiled and gave
-- a window that clamped at a wall that is not there.
--
-- @n@ is quantified before @v@, and both of these deliberately break the
-- \"vector first\" order the rest of the module uses. Nothing determines @n@
-- -- it appears only in the element type of the result list -- so the caller
-- always supplies it by type application, whereas @v@ is read off the argument.
-- The parameter that must be written comes first, so @gridWindows \@3@ works
-- rather than becoming @gridWindows \@_ \@3@. It is a 'Nat' and not a whole
-- axis type for the reason given on 'gridTiles': there is no longer a policy
-- to choose, only a size.
gridWindows :: forall n v big rest a.
               ( VG.Vector v a
               , AllSizedKnown rest
               , KnownNat n
               , n <= CoordNat big
               )
            => GridOf v (big ': rest) a
            -> [GridOf v (Ordinal n ': rest) a]
gridWindows (Grid v) =
    requiring @(n <= CoordNat big) $
    let restSize = fromIntegral $ natVal (Proxy @(MaxCoordSize rest))
        smallSize = fromIntegral $ natVal (Proxy @n)
        windowSize = smallSize * restSize
        bigSize = VG.length v `div` restSize
    in [ Grid (VG.slice (off * restSize) windowSize v)
       | off <- [0 .. bigSize - smallSize]
       ]
{-# INLINABLE gridWindows #-}

-- | 'gridWindows' as an optic -- and, on purpose, no more than a 'Fold'.
--
-- A window of size 3 over an axis of 9 puts cell 2 in three overlapping
-- windows (see 'gridWindows'). A 'Traversal'\'s foci must be
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
windows :: forall n v big rest a.
           ( VG.Vector v a
           , AllSizedKnown rest
           , KnownNat n
           , n <= CoordNat big
           )
        => Fold (GridOf v (big ': rest) a) (GridOf v (Ordinal n ': rest) a)
windows = folding (gridWindows @n)
{-# INLINABLE windows #-}
