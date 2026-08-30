# ylhl: a checked walker step

A typecheck-and-behaviour spike, not a benchmark, so unlike the other
directories here it is a single module with no cabal project of its own. It
runs inside the library's own repl:

```
cabal repl lib:grid-sized --repl-options=-ispike/ylhl-pointing
ghci> :add Spike
ghci> :m + Spike
ghci> ordinalWalk
[3,4,5]
ghci> policyWalks
Ordinal   : [((1,0),1),((1,1),1),((1,2),1),((1,3),1),((1,4),1)]
Clamped   : [((1,0),1),((1,1),1),((1,2),1),((1,3),1),((1,4),1)]
Periodic  : [((1,0),1),((1,1),1),((1,2),1),((1,3),1),((1,4),1),((1,0),1),...
Reflective: [((1,0),1),((1,1),1),((1,2),1),((1,3),1),((1,4),1)]
Reflect101: [((1,0),1),((1,1),1),((1,2),1),((1,3),1),((1,4),1)]
```

It answers two questions for `sized-grid-ylhl`, and finds a third thing:

1. **A checked step that carries a heading needs only `IsCoord` methods** --
   `offsetIsCoord` and `axisFrameFlipsIsCoord`, neither of which mentions
   `Diff`. So unlike `transportCoord` it is not excluded from `Ordinal`.

2. **A walker whose heading is indexed by `MapStep` can exist in a window.**
   Today's `Walker` cannot: its heading is `Diff (Coord cs)`, and
   `Diff (Ordinal n)` is stuck, so the field type has no values. `ordinalWalk`
   is the walk that type currently forbids -- across a 3x3 `Ordinal` board,
   stopping at the window's own edge instead of wrapping to the source's
   policy.

3. **`Reflective` and `Reflect101` disagreed about what a checked step
   means.** One stopped at the wall, the other turned around one cell early:
   `Reflect101` was the only axis in the library where a step the bounds check
   *accepts* also reported a frame flip, at the mirror cell where `mirrorAt`'s
   documented tie-break fired. Filed as `sized-grid-c0s9` and since fixed --
   the tie-break resolves both mirrors as not reflected, and the reading it
   was breaking is now a law on `axisFrameFlipsIsCoord`: *a checked step that
   succeeds has not hit a wall, so the frame does not turn*. The last row
   above is what the spike prints today; when it was written it read

   ```
   Reflect101: [((1,0),1),((1,1),1),((1,2),1),((1,3),1),((1,4),-1),((1,3),-1),...
   ```

   The design doc has the table.

The write-up is
[`docs/superpowers/specs/2026-08-29-pointing-family-design.md`](../../docs/superpowers/specs/2026-08-29-pointing-family-design.md).
