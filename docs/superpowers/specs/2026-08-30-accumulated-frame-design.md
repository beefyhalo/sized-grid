# The accumulated frame element

Design for `sized-grid-dse0`. Written 2026-08-30.

Companion to [the pointing family](2026-08-29-pointing-family-design.md), which
took `FocusedGrid` / `Walker` / the lifted movement operations. This is a
narrower question inside the same layer: what a walker carries about *which way
its own axes point*, and whether one parity bit is enough.

## Question

`sized-grid-pc93` landed `walkerFrameFlips :: Bool` on `Walker`
(`src/Data/Grid/Sized/Focused.hs:113`) --- the parity of the number of axes a
step reversed, i.e. the determinant of the frame transform. grid-atlas gives
the same one bit as `reversedFrame :: Crossing -> Bool`.

A consumer that reads direction keys in the walker's *own* frame needs more.
The walker's frame is an element of the group the seams (and reflecting walls)
generate, which for axis-aligned single-chart gluings is `(Z/2)^n` --- one
reversal bit per axis --- and the parity is only its determinant. On a Möbius
strip and a Klein bottle only one axis ever mirrors, so the determinant
determines the element and one bit is enough; on a projective plane both axes
mirror independently and it does not --- a sideways crossing swaps up/down, an
up/down crossing swaps left/right, and the parity bit cannot say which
happened.

Sokoban is the first consumer to write the whole thing by hand
(`sokoban/src/Sokoban/Board.hs`): `Turn { turnU, turnV }`, `square`,
`turnAfter :: Heading -> Crossing -> Turn -> Turn`,
`throughTurn :: Turn -> Heading -> Heading`, `headingFor`, `dirOf`. That is a
working 2-axis instance of what the library should own.

## Answer

**The library owns the group element, and the parity bit is derived from it.**

Three pieces, all in `Data.Grid.Sized.Coord.Transform` next to `axisFrameFlips`
and `transportCoord`:

1. `newtype Frame (cs :: [Type]) = Frame Int` --- the accumulator. One packed
   `Int`, bit `i` for the `i`th axis (outermost is bit 0), represented the way
   `Coord` represents its own group. `identityFrame` is the chart's frame;
   `Semigroup` / `Monoid` compose by componentwise `xor`; every element is its
   own inverse. `frameParity :: Frame cs -> Bool` is the determinant --- exactly
   the bit `walkerFrameFlips` and `reversedFrame` carry. `frameReversals` /
   `frameFromReversals` convert to and from `[Bool]`, outermost axis first.

2. `frameAfterStep :: Coord cs -> Diff (Coord cs) -> Frame cs -> Frame cs` ---
   compose one step into the accumulator. For each axis it xors in whether that
   step reversed *that axis's own* sense, which is what `Reflective` /
   `Reflect101` report through `axisFrameFlips` and every other axis type
   reports as `False`. This is `Data.Grid.Sized.Focused.stepFrameFlips` with the
   final xor-fold pulled out: `stepFrameFlips c d == frameParity (frameAfterStep
   c d identityFrame)`.

3. `throughFrame :: Frame cs -> Diff (Coord cs) -> Diff (Coord cs)` --- read a
   heading through the accumulated frame by negating its component on every
   reversed axis. Self-inverse. The `Diff`-level analogue of Sokoban's
   `throughTurn`; a player-frame view or an input reader turns a key press into
   a chart heading with this, and back with the same call.

### Why per-axis bits, and why `Int`

The group is `(Z/2)^n`: the subgroup of the hypercube's symmetries that
axis-aligned seams and reflecting walls can generate, with *no axis permuted*.
Its elements are exactly the `n`-bit vectors, composition is `xor`, the identity
is the zero vector. A packed `Int` is that, spelled the way the rest of the
library spells a per-axis fold result --- `Coord` is one `Int`, `stepFrameFlips`
already folds `Int -> NP I (MapDiff cs) -> Bool`. An `NP (K Bool) cs` would need
`All (Compose Eq (K Bool)) cs` to get `Eq`/`Show` and buys nothing the `Int`
does not. The constructor is not exported; `frameReversals` /
`frameFromReversals` are the boundary.

### What this is *not*

- **Not axis permutation.** A cube map turns a seam around a corner and maps one
  axis to another; its frame group is the full hyperoctahedral group, not
  `(Z/2)^n`. grid-atlas does not glue cube maps and `Frame` does not model them.
  When it does, `Frame` grows a permutation component --- it does not become a
  different type on the `Walker`.

- **Not the grid-atlas seam identity.** `sized-grid-c54t` (producer side) is
  about `grid-atlas`'s `Crossing` naming *which* axis a mirrored seam reflected,
  so a caller need not know that "mirror along A reverses the other axis" ---
  that identity holds because those charts glue an axis to itself. `Frame` here
  lives in the `transportCoord` world, where `axisFrameFlips` already reports
  each axis's flip against *that axis's own* displacement, so `frameAfterStep`
  needs no such identity. The two issues meet at the `Walker`, not in this type.

- **Not a new `Heading` type.** This library's heading is `Diff (Coord cs)`.
  `throughFrame` on `Diff` *is* the heading-level operation. Sokoban's
  `Heading` / `Dir` / `Extremum` vocabulary is a demo-side ergonomic layer
  (`sized-grid-f22w` territory), not something `Data.Grid.Sized` needs to own to
  close this issue.

## Landing

`Frame`, `identityFrame`, the `Semigroup`/`Monoid` instances, `frameParity`,
`frameReversals`, `frameFromReversals`, `frameAfterStep` and `throughFrame` land
now, standalone in `Coord.Transform`, re-exported from `Data.Grid.Sized.Coord`
and `Data.Grid.Sized`. They touch nothing that exists --- `Walker` still carries
`walkerFrameFlips :: Bool`.

Swapping the `Walker` field --- `walkerFrameFlips :: Bool` becomes
`walkerFrame :: Frame cs`, with `walkerFrameFlips` kept as
`frameParity . walkerFrame` for `sized-grid-pc93`'s consumers (the ant, which
wants only the parity) --- waits for `sized-grid-qbal`, which is already
reopening the `Walker` record to unstick the heading and add the
position-preserving steppers. As `dse0` says: a walker that reports failure, a
walker that reports parity and a walker that carries its frame are the same
walker, and the record should be opened once. That wiring is tracked in
`sized-grid-t8rw`.

Once the field is swapped, `Focused.stepFrameFlips` / `StepFrameFlips` collapse
to `frameParity . frameAfterStep`, removing the duplicated per-axis recursion.
Left in place for now so this change breaks nothing.

## The prior art, folded in (`sized-grid-qrxc`)

Sokoban's copy is gone. `Sokoban.Board` no longer defines `Turn`, `square` or
`throughTurn`: `Play` carries `playFrame :: Frame (Strip w h)`, a level starts
at `identityFrame`, and a key press is read through the accumulator by
`throughFrame`.

Which of the two seams the issue offered --- lower Sokoban's `Heading` through
`Diff`, or keep the `Heading` layer and touch `Frame` only where the
accumulator is composed --- was settled by *Not a new `Heading` type* above.
The demo keeps `Dir` / `Heading`, and a private pair `headingDelta` /
`deltaHeading` lowers a heading to the unit displacement it is for the one call
that needs it. `throughFrame` does the work; the `Heading` vocabulary is what
grid-atlas's steppers take and is not asked to leave.

One function stays behind, retyped and renamed `frameAfterCrossing`. It is not
`frameAfterStep` and cannot be: a `Strip`'s axes are both `Clamped` and neither
reflects, so `axisFrameFlips` reports nothing and the reflection arrives from
the gluing instead, as a `Crossing`. Turning that into a frame needs the
identity `a mirrored crossing along one axis reverses the other`, which holds
because these charts glue an axis to itself. That is the producer-side question
`sized-grid-c54t` asks grid-atlas to answer, and until it does, the identity
lives at the one call site that knows it is true.
