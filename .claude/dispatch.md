# Dispatch profile — grid-sized

Read by the global `/dispatch` skill. This file answers the two things that skill
must not guess for this repo: what the gate is, and what is too subtle to delegate.

## The gate

Run in the worktree, under `direnv exec` — outside the flake devshell `cabal`
fails on a missing toolchain and it reads as the worker's fault:

```bash
direnv exec "$wt" just test     # cabal test all --enable-tests, all seven suites
direnv exec "$wt" just lint     # hlint, reads .hlint.yaml at the root
```

`just build` alone is not a gate. `just check` (`nix flake check`, both GHC 9.12
and 9.14) is the stronger gate and worth running before landing anything that
touches the library rather than an example app — it is slow, so it is not the
default.

Formatting is ormolu and non-negotiable; the pre-commit hook enforces it. A diff
that reformats code the issue did not otherwise touch is a rejection.

## Routing

Route by type-level content, not by diff size. A three-line change to a
coordinate instance is supervisor work; a 300-line CI refactor is not.

**`agent:supervisor` — keep it.** Anything touching:

- `Data.Grid.Sized.Coord.*` — `IsCoord`, `IsCoordLifted`, `IsCoordList`, the
  boundary policies, and every `axis*` lifted wrapper that shadows a method.
- `Data.Grid.Sized.Internal.Grid.*` — `Core`, `Shape`, `Axis`, `Nest`, `Windows`.
- The shape algebra and its size proofs, any Nat-indexed class recursion (these
  need a fundep, not an associated type family — GHC rejects the overlap
  otherwise), `Optics.*`, `Stencil`, `Focused`, `Unsafe`.
- Any `type=decision` issue, and any issue whose description asks a question
  rather than naming a change.

**`agent:sonnet` — safe to delegate.** CI and workflow files, benchmark harness
plumbing (not benchmark *interpretation*), the example apps in `cabal.project`
(`sudoko`, `gameOfLife`, `ising-example`, `automata`, `maze`, `sokoban`,
`grid-atlas`, `atlas-topology`), Haddock and prose, metadata, test scaffolding.

**Performance issues are supervisor work even when the diff looks trivial.** The
benchmark numbers in this repo are only meaningful with the `-O` level of the
*caller* stated, and there are recorded traps a worker will walk into — see
`bd memories benchmark`.

## Dispatch with -s

`wt-bead -s <id> -- --model sonnet`. The `-s` is not optional here: CLAUDE.md
tells any agent in a worktree to `wt merge` its own branch once the gates are
green, and a worker that does that has skipped the gate and graded its own work.
`-s` suspends those two rules in the worker's prompt, and CLAUDE.md now carries
the matching carve-out. Observed once for real: the sized-grid-kqm8 worker
merged to master as 1f67194 before the supervisor could gate it (sized-grid-3vln).

## Serialisation

**Never run two benchmark issues at once.** Concurrent `cabal bench` across
worktrees makes every number in both runs worthless — the arms contend for the
same cores and cache, and the result is churn that reads as a regression. One
benchmark worker at a time, and let it finish before dispatching the next.

The same applies to the supervisor's own gate: do not run `just bench` while a
worker is running anything.

Benchmark runs also want the machine **on mains power**. macOS throttles on
battery, so a run started unplugged is not comparable with the baseline or with
a run made plugged in. Ask before starting one.

Non-benchmark workers can run in parallel, but check for **file overlap** first.
Two issues that touch the same file will conflict at merge even when both are
correct. Encode a known collision as a `blocks` dependency so the ready queue
stops offering them together — e.g. `bd dep add <later> <earlier> -t blocks`.

## Frozen

`spike/` is frozen ADR spikes. Excluded from hlint and ormolu; a diff touching it
is a rejection.

## Landing

```bash
cd "$wt" && wt merge --no-squash --no-rebase
```

`--no-rebase` is not optional: master carries merge commits and rebasing rewrites
history. Never `git pull --rebase` on master, never force-push. Fast-forward
only. Commit subjects end with the bead id in parens: `… (sized-grid-xxxx)`.

This is a solo repo — no pull requests, no waiting for review. Push master when
the gate is green.
