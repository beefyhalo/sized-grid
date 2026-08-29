{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The shape algebra: operations that rearrange, split, join or narrow a grid
-- along whole axes, leaving the elements alone.
--
-- All of it is arithmetic on the flat row-major vector -- a slice, a
-- concatenation, a backpermute -- because a whole-axis operation is a
-- contiguous run of blocks whatever the element type is. The one place a
-- coordinate appears is 'permuteGrid', where it is the caller's way of naming
-- the permutation.
--
-- == Which of these destroy the boundary policy
--
-- The rule is stated in full in "Data.Grid.Sized.Internal.Grid.Windows"'s
-- module header and on 'permuteGrid': __a restriction destroys the boundary
-- policy, a pointing preserves it.__ The operations here divide three ways.
--
-- /Restrictions/, which narrow an axis and so return
-- 'Data.Grid.Sized.Ordinal.Ordinal' along it whatever the source's policy
-- was: 'takeGrid', 'dropGrid', 'sliceGrid' and 'splitHigherDim'. Each is a
-- proper sub-window of the source axis wherever the size actually shrinks,
-- and a proper sub-window of a periodic axis is not periodic.
--
-- /Rearrangements/, which keep every axis at full width and so keep every
-- policy: 'transposeGrid', 'splitGrid', 'combineGrid', 'mapLowerDim' and
-- 'zipLowerDim'. 'splitGrid' is worth spelling out because it looks like a
-- restriction and is not: the outer @'Grid' \'[c]@ indexes the source's own
-- outermost axis at its own size, and each inner grid is full width in every
-- remaining axis, so nothing has been narrowed and both halves keep what they
-- had.
--
-- /Constructions/, where the policy is the caller's to declare because they
-- are asserting a topology rather than reading one off: 'combineHigherDim'
-- and 'permuteGrid'. See 'combineHigherDim' for why gluing two runs of cells
-- back together does not recover the axis they were cut from.
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
import           Data.Grid.Sized.Ordinal            (Ordinal)

import           Control.Applicative                (ZipList (..))
import           Data.Proxy                         (Proxy (..))
import qualified Data.Vector                        as V
import qualified Data.Vector.Generic                as VG
import           GHC.TypeLits
import qualified GHC.TypeLits                       as GHC

-- | The general "view a grid by reindexing it", and --- despite the name ---
-- not restricted to permutations.
--
-- Nothing requires @f@ to be bijective, injective, or even non-constant. It is
-- 'GridOf' being contravariant in its index, and every restriction in
-- "Data.Grid.Sized.Internal.Grid.Windows" is an instance of it: a window of 3
-- at offset 1 is @permuteGrid (\\c -> unsafeOrdinal (ordinalToInt (headOf c) + 1)
-- :| EmptyCoord)@. The specialised ones exist because each is one 'VG.slice'
-- where this builds a @'coordSpaceSize' \@cs@-long index table and a 'Coord'
-- per cell; the concept has its home here.
--
-- Being the general form, it is where the boundary-policy rule is stated in
-- full: __a restriction destroys the boundary policy, a pointing preserves
-- it.__ @cs@ and @ds@ are independent, so @f@ may relabel a @Periodic 9@ axis
-- as a @Clamped 3@ one and this will do it --- which is right, because @f@ is
-- the caller writing down exactly what the new space is and how it maps into
-- the old one. It is also the whole of the obligation: whatever @cs@ says about
-- walls and wrapping is what callers of the result will get, and a proper
-- sub-window of a periodic axis is not periodic. Where the extent narrows and
-- @f@ is not onto, @'Data.Grid.Sized.Ordinal.Ordinal'@ is the axis that tells
-- the truth about it. @permuteGrid@ cannot check this and does not try; the
-- named restrictions in "Data.Grid.Sized.Internal.Grid.Windows" have it in
-- their types instead, which is the reason to prefer one when it fits.
--
-- The name is kept because it is published. @reindexGrid@ would describe it
-- better.
--
-- @tabulate (index g . f)@ for a coordinate endomorphism-or-relabelling @f@
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

-- | Glue two grids along the outermost axis. The result's policy is whatever
-- the two halves already carry, and that is deliberate: this is a
-- construction, and a construction is where the caller declares a topology
-- rather than reading one off.
--
-- The consequence, once 'splitHigherDim' returns
-- 'Data.Grid.Sized.Ordinal.Ordinal' halves (sized-grid-pnws), is that
-- split-then-recombine does not give back the axis it started from: a
-- @Periodic 9@ splits into two runs of cells and gluing them yields
-- @Ordinal 9@, not @Periodic 9@. That is the honest answer. Whether cell 8 is
-- adjacent to cell 0 is not a fact about either run; it is a fact about the
-- space they were cut from, and it does not survive the cut. A caller who
-- wants it back is asserting it, and asserts it with 'permuteGrid' at
-- @'unsafeCoordFromPosition' . \'coordPosition\'@ or by rebuilding through
-- 'Data.Grid.Sized.gridFromVector'.
--
-- So this is /not/ generalised to a free result axis with
-- @CoordNat c ~ CoordNat a + CoordNat b@. That would let the assertion be
-- made here, in the glue, which reads as though the policy were recovered
-- rather than restated -- and it would leave @c@ undetermined at every call
-- site whose result type is not already pinned, turning an inference failure
-- into the common case for no correctness gain.
--
-- @IsCoord c@ used to be demanded here. It buys nothing: the size of a coord
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
-- The remainder is 'Data.Grid.Sized.Ordinal.Ordinal'-axed whatever @c@ was,
-- per the module header. Its low end is a cut, not an edge of the source, so
-- a @Clamped@ remainder would stand still at a wall that is not there and a
-- @Periodic@ one would wrap round a fraction of its own cycle. Nothing is
-- asked of @c@ at all now, which is the statement that a restriction does not
-- care what policy it is restricting.
--
-- @n <= m@ is required: without it @dropGrid 9@ of a 3-grid typechecked and
-- produced a grid whose vector was empty while its type claimed @3 - 9@.
dropGrid ::
       forall v m c x. forall n -> (VG.Vector v x, KnownNat n, n <= m)
    => GridOf v '[ c m] x
    -> GridOf v '[ Ordinal (m - n)] x
dropGrid n (Grid v) =
    requiring @(n <= m) $ Grid $ VG.drop (fromIntegral $ natVal (Proxy @n)) v
{-# INLINABLE dropGrid #-}

-- | Keep the first @n@ elements of a one-dimensional grid:
--
-- > takeGrid 2 g   -- rather than takeGrid (Proxy @2) g
--
-- 'Data.Grid.Sized.Ordinal.Ordinal'-axed for the reason given on 'dropGrid':
-- the prefix keeps the source's low end but its high end is a cut, so
-- @takeGrid 3@ of a @Grid \'[Periodic 9]@ used to hand back a
-- @Grid \'[Periodic 3]@ that wrapped round its own three cells.
--
-- @n <= m@ is required: 'VG.take' cannot conjure elements, so without the
-- constraint @takeGrid 9@ of a 3-grid returned 3 elements under a type that
-- promised 9.
takeGrid ::
       forall v m c x. forall n -> (VG.Vector v x, KnownNat n, n <= m)
    => GridOf v '[ c m] x
    -> GridOf v '[ Ordinal n] x
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
--
-- The window is 'Data.Grid.Sized.Ordinal.Ordinal'-axed whatever @c@ was, per
-- the module header; this is the general case that 'takeGrid' and 'dropGrid'
-- are the two ends of, and neither of its ends need be an end of the source.
-- @shrinkGrid@ used to restate this by hand, through a @forgetAxisPolicy@ in
-- "Data.Grid.Sized.Internal.Grid.Windows" that existed only because this
-- function preserved the constructor; that helper is gone.
sliceGrid ::
       forall v m c x. forall off len -> ( VG.Vector v x
                                         , KnownNat off
                                         , KnownNat len
                                         , off + len <= m)
    => GridOf v '[ c m] x
    -> GridOf v '[ Ordinal len] x
sliceGrid off len (Grid v) =
    requiring @(off + len <= m) $
    Grid $
    VG.slice
        (fromIntegral $ natVal (Proxy @off))
        (fromIntegral $ natVal (Proxy @len))
        v
{-# INLINABLE sliceGrid #-}

-- | Cut a grid in two along its outermost axis.
--
-- Both halves are 'Data.Grid.Sized.Ordinal.Ordinal'-axed whatever @c@ was:
-- the cut is an edge of neither piece's source, so each piece would otherwise
-- claim a wall or a wrap there that the grid it came from does not have. The
-- remaining axes @as@ are untouched at full width and keep their policies,
-- because they have not been restricted.
--
-- This does not round-trip through 'combineHigherDim' back to @c@, and that
-- is the point rather than a gap; see that function.
--
-- The second component is @x - y@, not a free type variable. It used to be
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
    -> (GridOf v (Ordinal y ': as) a, GridOf v (Ordinal (x - y) ': as) a)
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
