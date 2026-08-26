{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The shape algebra: operations that rearrange, split, join or narrow a grid
-- along whole axes, leaving the elements alone.
--
-- All of it is arithmetic on the flat row-major vector -- a slice, a
-- concatenation, a backpermute -- because a whole-axis operation is a
-- contiguous run of blocks whatever the element type is. The one place a
-- coordinate appears is 'permuteGrid', where it is the caller's way of naming
-- the permutation.
module Data.Grid.Sized.Internal.Grid.Shape
  ( -- * Rearranging
    permuteGrid
  , transposeGrid
  , splitGrid
  , combineGrid
  , combineHigherDim
  , splitHigherDim
  , dropGrid
  , takeGrid
  , sliceGrid
  , mapLowerDim
  , zipLowerDim
    -- * Chunked traversal
  , traverseChunks
  ) where

import           Data.Grid.Sized.Coord
import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Internal.Grid.Core
import           Data.Grid.Sized.Internal.Type      (requiring)

import           Control.Applicative                (ZipList (..))
import           Data.Proxy                         (Proxy (..))
import qualified Data.Vector                        as V
import qualified Data.Vector.Generic                as VG
import           GHC.TypeLits
import qualified GHC.TypeLits                       as GHC

-- | @tabulate (index g . f)@ for a coordinate endomorphism-or-relabelling @f@
-- is a permutation of the underlying vector: which source position feeds
-- which target position depends only on @cs@, @ds@ and @f@, never on @g@'s
-- elements. So it can be computed once as a table of positions and applied
-- with 'VG.unsafeBackpermute', a tight loop with no @Coord@ built, permuted
-- or destroyed per cell -- unlike @tabulateGrid (indexGrid g . f)@, which is
-- exactly that per cell. 'transposeGrid' is this at a fixed @f@.
--
-- INLINE, not just INLINABLE. An offered-but-declined unfolding leaves @f@,
-- @cs@ and @ds@ opaque to whichever module actually calls 'permuteGrid' --
-- this library's own benchmark executable among them -- and the coordinate
-- machinery behind 'coordPosition' and 'allCoord' only unrolls into flat
-- arithmetic when the axis list is known at the point that inlines it.
-- Measured at INLINABLE: 22-29 ms and 85-92 MB for 'transposeGrid' at
-- 300x300, an order of magnitude worse than the per-cell 'tabulateGrid' this
-- is meant to beat, and unchanged by raising the whole project to -O2 -- so
-- it was not an optimisation-level problem, it was this unfolding never
-- being offered anywhere the coordinate machinery could use it. Forcing it
-- with INLINE dropped the same benchmark to 873 μs / 704 KB boxed and
-- 368 μs / 703 KB unboxed -- 82% under the per-cell baseline on both, per
-- @bench/baseline-ghc9.12.3-aarch64-darwin.csv@.
--
-- 'VG.unsafeBackpermute': every entry of the table is @coordPosition (f c)@
-- for some real @Coord cs@ @c@, so by the same argument as 'indexGrid' it
-- lands in @[0, MaxCoordSize ds)@ -- which is @VG.length@ of the source
-- vector, by the `GridOf` size invariant. The bounds check
-- 'VG.backpermute' would do can never fire.
--
-- A caller cannot supply a bad permutation: they supply a coordinate
-- function, and @Coord ds@ is only inhabited by in-range coordinates, so
-- whatever @f@ returns is safe to look up.
-- @IsCoordList ds@ is likewise gone: the table is @coordPosition@ of whatever
-- @f@ returns, and that is now a field read rather than a fold.
permuteGrid ::
       forall v cs ds a.
       (VG.Vector v a, VG.Vector v Int, IsCoordList cs)
    => (Coord cs -> Coord ds)
    -> GridOf v ds a
    -> GridOf v cs a
permuteGrid f (Grid v) = Grid (VG.unsafeBackpermute v idx)
  where
    idx = VG.fromListN (coordSpaceSize @cs) $ map (coordPosition . f) allCoord
{-# INLINE permuteGrid #-}

transposeGrid ::
     ( VG.Vector v a
     , VG.Vector v Int
     , IsCoord h
     , IsCoord w
     , GHC.KnownNat x
     , GHC.KnownNat y
     , 1 <= y
     , 1 <= x
     )
  => GridOf v '[ w x, h y] a
  -> GridOf v '[ h y, w x] a
transposeGrid = permuteGrid transposeCoord
{-# INLINABLE transposeGrid #-}

-- | The outer grid holds grids, and a grid is never an unboxed element, so the
-- outer vector is boxed whatever @v@ is. That asymmetry is what makes the whole
-- shape algebra shareable: only the /inner/ representation follows @v@, and
-- 'combineGrid' puts it back.
splitGrid ::
       forall v c cs a. (VG.Vector v a, AllSizedKnown cs)
    => GridOf v (c ': cs) a
    -> Grid '[ c] (GridOf v cs a)
splitGrid (Grid v) =
    Grid $
    V.fromList $
    map
        Grid
        (splitVectorBySize
             (fromIntegral $ GHC.natVal (Proxy :: Proxy (MaxCoordSize cs)))
             v)
{-# INLINABLE splitGrid #-}

combineGrid ::
       forall v c cs a. VG.Vector v a
    => Grid '[ c] (GridOf v cs a)
    -> GridOf v (c ': cs) a
combineGrid (Grid v) = Grid $ VG.concat $ map unGrid $ V.toList v
{-# INLINE combineGrid #-}

-- | @IsCoord c@ used to be demanded here. It buys nothing: the size of a coord
-- comes from @CoordNat@ on the `Data.Grid.Sized.Coord.Class.IsCoordLifted` instance,
-- not from `IsCoord`, so the class could not have justified the @n + m@ in the
-- result even in principle.
combineHigherDim ::
       VG.Vector v x
    => GridOf v (c n ': as) x
    -> GridOf v (c m ': as) x
    -> GridOf v (c (n + m) ': as) x
combineHigherDim (Grid v1) (Grid v2) = Grid (v1 VG.++ v2)
{-# INLINE combineHigherDim #-}

-- | Drop the first @n@ elements of a one-dimensional grid:
--
-- > dropGrid 2 g   -- rather than dropGrid (Proxy @2) g
--
-- @n <= m@ is required: without it @dropGrid 9@ of a 3-grid typechecked and
-- produced a grid whose vector was empty while its type claimed @3 - 9@.
dropGrid ::
       forall v m c x. forall n -> (VG.Vector v x, KnownNat n, n <= m)
    => GridOf v '[ c m] x
    -> GridOf v '[ c (m - n)] x
dropGrid n (Grid v) =
    requiring @(n <= m) $ Grid $ VG.drop (fromIntegral $ natVal (Proxy @n)) v
{-# INLINABLE dropGrid #-}

-- | Keep the first @n@ elements of a one-dimensional grid:
--
-- > takeGrid 2 g   -- rather than takeGrid (Proxy @2) g
--
-- @n <= m@ is required: 'VG.take' cannot conjure elements, so without the
-- constraint @takeGrid 9@ of a 3-grid returned 3 elements under a type that
-- promised 9.
takeGrid ::
       forall v m c x. forall n -> (VG.Vector v x, KnownNat n, n <= m)
    => GridOf v '[ c m] x
    -> GridOf v '[ c n] x
takeGrid n (Grid v) =
    requiring @(n <= m) $ Grid $ VG.take (fromIntegral $ natVal (Proxy @n)) v
{-# INLINABLE takeGrid #-}

-- | Keep @len@ elements of a one-dimensional grid starting at offset @off@:
--
-- > sliceGrid 1 2 g   -- elements 1 and 2 of a 3-grid
--
-- This is @takeGrid len . dropGrid off@ with the intermediate size fused
-- away. Composed, the two state the window bound as @len <= m - off@ over
-- GHC's truncating subtraction, out of reach of ghc-typelits-natnormalise
-- once @off@ is an existential (as it is in @shrinkGrid@, from
-- 'reifyCoord'). Written @off + len <= m@ instead, it's ordinary linear
-- arithmetic the solver discharges directly.
--
-- @off + len <= m@ is also precisely 'VG.slice'\'s own precondition, so the
-- bounds check it performs can never fire here.
sliceGrid ::
       forall v m c x. forall off len -> ( VG.Vector v x
                                         , KnownNat off
                                         , KnownNat len
                                         , off + len <= m)
    => GridOf v '[ c m] x
    -> GridOf v '[ c len] x
sliceGrid off len (Grid v) =
    requiring @(off + len <= m) $
    Grid $
    VG.slice
        (fromIntegral $ natVal (Proxy @off))
        (fromIntegral $ natVal (Proxy @len))
        v
{-# INLINABLE sliceGrid #-}

-- | The second component is @x - y@, not a free type variable. It used to be
-- free, which let the caller annotate the remainder with any size at all and
-- get a grid whose vector did not match.
splitHigherDim ::
       forall v c as x y a.
       ( VG.Vector v a
       , KnownNat y
       , y <= x
       , AllSizedKnown as
       )
    => GridOf v (c x ': as) a
    -> (GridOf v (c y ': as) a, GridOf v (c (x - y) ': as) a)
splitHigherDim (Grid v) =
    requiring @(y <= x) $
    let (a, b) =
            VG.splitAt
                (fromIntegral $
                 GHC.natVal (Proxy @y) * GHC.natVal (Proxy @(MaxCoordSize as)))
                v
     in (Grid a, Grid b)
{-# INLINABLE splitHigherDim #-}

-- | Split a grid into its @CoordNat c@ sub-grids along the outermost axis,
-- apply @f@ to each, and glue the results back together.
--
-- The effects of @f@ are combined with @traverse@, so the choice of @f@ decides
-- how the per-sub-grid results are combined, and the obvious choice is usually
-- the wrong one. With @f ~ []@ this is the list applicative -- a cartesian
-- product of one result per sub-grid, @n ^ n@ grids for @n@ sub-grids each
-- returning @n@ results, not @n@ grids. That is almost never what a caller
-- taking slices means; use 'zipLowerDim' for that. @f ~ Identity@ (a
-- length-preserving map over each sub-grid) and @f ~ Maybe@ (a fallible one)
-- behave as expected.
--
-- The element type may change, so both @v x@ and @v y@ have to be vectors.
mapLowerDim ::
       forall v as bs x y c f.
       (VG.Vector v x, VG.Vector v y, AllSizedKnown as, Applicative f)
    => (GridOf v as x -> f (GridOf v bs y))
    -> GridOf v (c ': as) x
    -> f (GridOf v (c ': bs) y)
mapLowerDim f (Grid v) =
    Grid <$>
    traverseChunks (fromIntegral (GHC.natVal (Proxy @(MaxCoordSize as)))) f v
{-# INLINABLE mapLowerDim #-}

-- | The engine shared by 'mapLowerDim' and @tiles@: split a flat vector into
-- equally sized chunks, traverse each chunk as a sub-grid, and concatenate the
-- results back into one vector. The two callers differ only in how the chunk
-- size is computed -- @'MaxCoordSize' as@ for 'mapLowerDim', where the chunk
-- count is fixed by the axis @c@ being peeled off, versus @'MaxCoordSize'
-- (small ': rest)@ for @tiles@, where the chunk size is fixed and the count
-- falls out of the vector's length.
traverseChunks ::
     forall v x y as bs f. (VG.Vector v x, VG.Vector v y, Applicative f)
  => Int
  -> (GridOf v as x -> f (GridOf v bs y))
  -> v x
  -> f (v y)
traverseChunks size f =
    fmap VG.concat . traverse (fmap unGrid . f . Grid) . splitVectorBySize size
{-# INLINE traverseChunks #-}

-- | 'mapLowerDim' where @f@ returns many results per sub-grid and they should
-- be zipped positionally rather than multiplied: the @k@th result is built from
-- the @k@th result of every sub-grid.
--
-- This is what slicing a grid along its second axis needs. @zipLowerDim
-- @gridTiles@@ on a 9x9 board gives the 9 columns; @mapLowerDim @gridTiles@@
-- gives 387,420,489 grids, one for every way of picking a cell from each row.
--
-- The result is as long as the shortest per-sub-grid list, so it is only the
-- expected length when @f@ returns the same number of results for every
-- sub-grid -- which @gridTiles@ does, since its count is fixed by the types.
zipLowerDim ::
       forall v as bs x y c. (VG.Vector v x, VG.Vector v y, AllSizedKnown as)
    => (GridOf v as x -> [GridOf v bs y])
    -> GridOf v (c ': as) x
    -> [GridOf v (c ': bs) y]
zipLowerDim f = getZipList . mapLowerDim (ZipList . f)
{-# INLINABLE zipLowerDim #-}
