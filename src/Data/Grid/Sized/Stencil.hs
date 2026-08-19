-- |
-- Module      :  Data.Grid.Sized.Stencil
-- License     :  MIT -style (see the file LICENSE)
--
-- A neighbourhood, precomputed once for a grid /type/ and then run over as many
-- grids of that type as you like.
--
-- The shape of a cellular-automaton step is
--
-- > imapGrid (\c here -> rule here (map (indexGrid g) (neighbours c))) g
--
-- and the cost of it is not the rule. Per cell it walks the axis list to build
-- a @Coord@ per neighbour --- an @NP I cs@ spine of boxes --- applies each
-- axis's boundary policy to decide whether that neighbour exists, and then
-- throws the whole coordinate away again through `coordPosition` to get the one
-- `Int` the vector is actually read at.
--
-- None of that depends on the grid. For a fixed axis list and radius, /which
-- vector positions are the neighbours of position i/ is a function of the type
-- alone: the axis sizes fix the strides, and the per-axis boundary policies fix
-- what happens at an edge. A `Stencil` is that function, computed once and
-- stored flat.
--
-- == The table
--
-- A @'Stencil' cs@ holds @'coordSpaceSize' \@cs * 'stencilWidth'@ positions in
-- one unboxed vector, row @i@ being the neighbours of position @i@. The width
-- is the largest neighbour count any cell has; cells with fewer --- which on a
-- bounded axis is every cell near an edge --- are padded with a sentinel, and
-- because the padding is a suffix, `stencilGrid` stops at the first one rather
-- than scanning past it. A grid all of whose axes are
-- `Data.Grid.Sized.Coord.Periodic.Periodic` has no short rows at all.
--
-- == Why you build one and hold it
--
-- Building the table costs one full pass of the very neighbourhood computation
-- it replaces --- rather more than one, because it also lays out a vector --- so
-- a `Stencil` used once is slower than not having bothered. Measured on the
-- @50 x 50@ automaton step in @bench\/Main.hs@, against the
-- @imapGrid@-over-@neighbours@ loop above rather than against
-- @extend neighbourSum@, so that the comparison is the neighbourhood and not
-- also a `Data.Grid.Sized.Focused.FocusedGrid` the rule never reads:
--
-- * the loop, one pass: 3.3 ms, 16 MB
-- * building the table: 5.7 ms, 14 MB
-- * one pass with the table already built: 0.28 ms, 2.4 MB
--
-- So a single pass through a stencil is about 1.9x slower than not bothering,
-- the second pass is where it breaks even, and from there each one is 12x
-- cheaper and allocates a seventh as much. Which is what an automaton does:
--
-- > let s = mooreStencil 1
-- > in iterate (stencilGrid s rule) g !! 100
--
-- measured at 65 ms and 237 MB against 361 ms and 1.5 GB for the loop it
-- replaces.
--
-- That is why the table is a value rather than an argument to `stencilGrid`.
-- Written as @stencilGrid 1 rule g@ the sharing would have to come from GHC
-- floating the table out to a CAF, which happens only where the axis-list
-- dictionary is known statically and, when it does happen, retains the table
-- for the lifetime of the process. As a value the sharing is visible in the
-- code that wants it and ends when that code drops it.
--
-- == What it does not change
--
-- The boundary policies. The table is built by asking the same
-- `Data.Grid.Sized.Coord.mooreNeighbours` the hand-written loop asks, one cell
-- at a time, so a `Data.Grid.Sized.Coord.Periodic.Periodic` axis wraps and a
-- `Data.Grid.Sized.Coord.Clamped.Clamped` one drops, exactly and only as
-- before. @Test.Stencil@ states that as a property over mixed-policy axis
-- lists.
module Data.Grid.Sized.Stencil
  ( -- * The table
    Stencil
  , stencilWidth
  , stencilPositions
    -- * Building one
  , stencilFor
  , mooreStencil
  , vonNeumannStencil
    -- * Running one
  , stencilGrid
  , stencilFoldGrid
  , stencilAt
  ) where

import           Data.Grid.Sized.Coord
import           Data.Grid.Sized.Internal.Grid (GridOf, gridVector,
                                                unsafeGridFromVector)

import           Data.Kind                     (Type)
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Unboxed           as VU

-- | The neighbourhood of every cell of a @cs@-shaped grid, as vector positions.
--
-- Indexed by the axis list and nothing else --- not by the vector type, not by
-- the element type --- because that is the whole claim: two grids of the same
-- shape have the same neighbour positions whatever they hold.
--
-- Abstract. The two accessors below are for inspecting one; there is no
-- constructor, because a table whose width and length disagree, or whose
-- positions are out of range, would make `stencilGrid` read out of bounds.
data Stencil (cs :: [Type]) = Stencil
    { stencilWidth     :: !Int
      -- ^ The largest number of neighbours any cell has, and so the stride of
      -- 'stencilPositions'. Rows shorter than this are padded with @-1@.
    , stencilPositions :: !(VU.Vector Int)
      -- ^ @'coordSpaceSize' \@cs * 'stencilWidth'@ entries, row-major by cell:
      -- the neighbours of position @i@ occupy
      -- @[i * w, (i + 1) * w)@, ending at the first @-1@ if there is one.
    }

-- | Precompute a neighbourhood given as a function of the coordinate.
--
-- The general constructor: 'mooreStencil' and 'vonNeumannStencil' are this
-- applied to the two neighbourhoods the library already defines, and anything
-- else a caller can write --- a knight's move, one axis only, an asymmetric
-- kernel --- goes through here without needing a new entry point.
--
-- The function is called once per cell, at build time, and its results are
-- taken as given: whatever coordinates it returns become the neighbours, in the
-- order it returned them. It is therefore the /only/ place a boundary policy is
-- consulted, which is what makes 'stencilGrid' agree with the loop it replaces
-- by construction rather than by a second implementation of the same rules.
--
-- The width is discovered rather than derived from the radius, because a
-- derived bound would be wrong in both directions: 'Data.Grid.Sized.Coord.axisSteps'
-- drops neighbours a bounded axis refuses /and/ deduplicates the ones a small
-- periodic or reflecting axis reaches twice, so @(2r+1)^d - 1@ is an
-- over-estimate that would pad every row with sentinels the inner loop then has
-- to walk.
stencilFor ::
       forall cs. IsCoordList cs
    => (Coord cs -> [Coord cs])
    -> Stencil cs
stencilFor neighbourhood = Stencil w (VU.fromListN (n * w) (concatMap pad rows))
  where
    n = coordSpaceSize @cs
    -- Held live across the width pass rather than computed twice: the second
    -- pass would be the coordinate work this whole module exists to do once.
    rows = [map coordPosition (neighbourhood c) | c <- allCoord @cs]
    w = maximum (0 : map length rows)
    pad ps = take w (ps ++ repeat (-1))
{-# INLINABLE stencilFor #-}

-- | The Moore neighbourhood at the given radius: `Data.Grid.Sized.Coord.mooreNeighbours`
-- precomputed. @mooreStencil 1@ is the game-of-life neighbourhood, diagonals
-- included.
mooreStencil :: forall cs. IsCoordList cs => Int -> Stencil cs
mooreStencil r = stencilFor (mooreNeighbours r)
{-# INLINABLE mooreStencil #-}

-- | The von Neumann neighbourhood at the given radius:
-- `Data.Grid.Sized.Coord.vonNeumannNeighbours` precomputed. @vonNeumannStencil 1@
-- is the orthogonally adjacent cells, however many dimensions there are.
vonNeumannStencil :: forall cs. IsCoordList cs => Int -> Stencil cs
vonNeumannStencil r = stencilFor (vonNeumannNeighbours r)
{-# INLINABLE vonNeumannStencil #-}

-- | Rebuild a grid from each cell and its neighbours.
--
-- @stencilGrid s f g@ is @f here neigh@ at every position, where @neigh@ is
-- what the stencil says the neighbours are, read out of the /old/ grid. So it
-- is the bulk automaton step, and unlike `Control.Comonad.extend` it needs no
-- grid of grids and works on an unboxed grid.
--
-- Note what is missing from the signature: no @IsCoordList cs@. Every use of
-- the axis list happened when the `Stencil` was built, and what is left here is
-- a loop over two vectors. That is the point of the exercise stated as a type.
--
-- The neighbour list is produced lazily, so a rule that stops early --- a
-- @take@, a short-circuiting fold --- does not pay for the rest of it. See
-- 'gatherRow' for why reading it needs no bounds check.
stencilGrid ::
       forall v cs a b. (VG.Vector v a, VG.Vector v b)
    => Stencil cs
    -> (a -> [a] -> b)
    -> GridOf v cs a
    -> GridOf v cs b
stencilGrid (Stencil w tbl) f g =
    unsafeGridFromVector $
    VG.generate n $ \i -> f (VG.unsafeIndex v i) (gatherRow tbl w v i)
  where
    v = gridVector g
    -- The grid's own length rather than 'coordSpaceSize', which are the same
    -- number by the size invariant. Taking it from the vector is what lets this
    -- function go without the 'IsCoordList' constraint that would otherwise be
    -- needed only to recompute a length already in hand.
    n = VG.length v
{-# INLINABLE stencilGrid #-}

-- | Rebuild a grid from each cell and its neighbours, folded in place rather
-- than gathered into a list first.
--
-- @stencilFoldGrid s step seed g@ is @foldl' step (seed here) neigh@ at every
-- position, for the same @neigh@ `stencilGrid` would hand its rule as a list
-- --- so @stencilFoldGrid s step seed@ agrees with
-- @stencilGrid s (\\here ns -> foldl' step (seed here) ns)@ everywhere,
-- `Test.Stencil` states that as a property.
--
-- Where `stencilGrid` builds a @[a]@ per cell --- one cons cell and one boxed
-- element per neighbour, thrown away as soon as a fold like a neighbour-sum
-- rule folds it right back down --- this reads each neighbour out of the
-- table and folds it into the accumulator directly, so that list is never
-- built. `gatherRow` is still the one place the row's span and sentinel are
-- decided; `foldRow'` walks the same span the same way, just strictly and
-- without consing.
--
-- Keep `stencilGrid` for rules that do not fold every neighbour every time
-- --- a @take@, a short-circuiting @any@ --- `gatherRow`'s laziness is what
-- lets those stop early, and a strict fold cannot; that is why
-- @gameOfLife@'s @runRule@ stays on `stencilGrid`.
--
-- Measured on the same @50 x 50@ neighbour-sum step as `stencilGrid`'s own
-- Haddock, in @bench\/Main.hs@:
--
-- * one pass, table already built: 283 μs and 2.4 MB for `stencilGrid`
--   against 92 μs and 479 KB here --- 3x faster and 5x less allocation.
-- * @iterate (stencilFoldGrid s (+) id) g !! 100@: 41 ms and 47 MB against
--   70 ms and 237 MB for the `stencilGrid` loop it replaces.
-- * the same x100 loop on an unboxed grid: 19 ms and 149 MB against 25 ms
--   and 249 MB. Boxed and unboxed allocate almost the same amount for
--   `stencilGrid` --- 237 MB against 249 MB --- which is the answer to the
--   question this issue was opened to settle: that allocation is almost
--   entirely the neighbour lists, not boxed-@Int@ thunk chains, because a
--   list element is boxed on the way out of an unboxed vector regardless.
stencilFoldGrid ::
       forall v cs a b. (VG.Vector v a, VG.Vector v b)
    => Stencil cs
    -> (b -> a -> b)
    -> (a -> b)
    -> GridOf v cs a
    -> GridOf v cs b
stencilFoldGrid (Stencil w tbl) step seed g =
    unsafeGridFromVector $
    VG.generate n $ \i -> foldRow' tbl w v i step (seed (VG.unsafeIndex v i))
  where
    v = gridVector g
    n = VG.length v
{-# INLINABLE stencilFoldGrid #-}

-- | The neighbours of a single cell, read out of the table rather than
-- enumerated.
--
-- @stencilAt s g c@ is @map (indexGrid g) (neighbourhood c)@ for the
-- neighbourhood @s@ was built from, and it exists because not every consumer
-- rewrites the whole grid. A single-site Metropolis step --- the shape
-- @ising-example@ runs --- picks one cell at random, reads its neighbourhood,
-- and puts back one element; running 'stencilGrid' for that would compute every
-- other cell to throw it away.
--
-- Unlike 'stencilGrid' this does need @IsCoordList cs@, to turn the coordinate
-- into the row of the table it names. That single `coordPosition` is all that is
-- left of the axis-list work: what it saves is the per-neighbour coordinate
-- arithmetic and the boundary policy behind it, not the one position lookup.
stencilAt ::
       forall v cs a. (VG.Vector v a, IsCoordList cs)
    => Stencil cs
    -> GridOf v cs a
    -> Coord cs
    -> [a]
stencilAt (Stencil w tbl) g c = gatherRow tbl w (gridVector g) (coordPosition c)
{-# INLINABLE stencilAt #-}

-- | The half-open span of 'stencilPositions' that holds row @i@ of a stencil
-- of width @w@: @[i * w, (i + 1) * w)@. The one piece of table-layout
-- arithmetic `gatherRow` and `foldRow'` would otherwise each restate.
rowSpan :: Int -> Int -> (Int, Int)
rowSpan w i = (i * w, (i + 1) * w)
{-# INLINE rowSpan #-}

-- | Row @i@ of the table, as the elements it names.
--
-- The sentinels are a suffix of each row, so meeting one ends the row and a
-- full row simply reaches the end of its span first. Lazy in the tail, so a
-- rule that stops early does not read the neighbours it never asked for.
--
-- 'VG.unsafeIndex' throughout, licensed by the same invariant
-- `Data.Grid.Sized.indexGrid` relies on: every entry of the table came from
-- `coordPosition` on a real @'Coord' cs@, so it is in
-- @[0, 'coordSpaceSize' \@cs)@, and the grid has exactly that many elements.
-- Both halves are facts about @cs@, which is why the stencil is indexed by it.
gatherRow :: VG.Vector v a => VU.Vector Int -> Int -> v a -> Int -> [a]
gatherRow tbl w v i = go start
  where
    (start, end) = rowSpan w i
    go !j
        | j >= end = []
        | otherwise =
            case VU.unsafeIndex tbl j of
                -1 -> []
                p  -> VG.unsafeIndex v p : go (j + 1)
{-# INLINE gatherRow #-}

-- | Row @i@ of the table, folded left instead of gathered into a list.
--
-- Same span, same sentinel, same licence for 'VG.unsafeIndex' as `gatherRow`
-- --- see there for why every entry is in range. Strict in the accumulator, so
-- `stencilFoldGrid` never builds a thunk chain across a row; the one it cannot
-- avoid is the outer 'VG.generate', which `stencilGrid` pays too.
foldRow' :: VG.Vector v a => VU.Vector Int -> Int -> v a -> Int -> (b -> a -> b) -> b -> b
foldRow' tbl w v i step = go start
  where
    (start, end) = rowSpan w i
    go !j !acc
        | j >= end = acc
        | otherwise =
            case VU.unsafeIndex tbl j of
                -1 -> acc
                p  -> go (j + 1) (step acc (VG.unsafeIndex v p))
{-# INLINE foldRow' #-}
