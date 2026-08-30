# Project Instructions for AI Agents

This file provides instructions and context for AI coding agents working on this project.

<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:ca08a54f -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

## Session Completion

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd dolt push
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
<!-- END BEADS INTEGRATION -->


## Environment & Toolchain

The toolchain comes from the Nix flake dev shell, loaded automatically by direnv
(`.envrc` is `use flake`). Outside direnv, run commands under `nix develop -c …`.

- Default compiler is **GHC 9.12** (`devShells.default`), matching `../aoc` so the
  shared `cabal.project` does not recompile the world. GHC 9.14 is kept building:
  `nix develop .#ghc914`, `packages.grid-sized-ghc914`.
- The shell provides `cabal`, `haskell-language-server`, `ormolu`, `hlint`, and
  installs the git pre-commit hook (hlint + ormolu) on first entry.
- Type-checker plugins (`ghc-typelits-natnormalise`, `ghc-typelits-knownnat`) are
  pinned from Hackage in `flake.nix` — read the comments there before bumping.

## Worktrees

This repo uses **worktrunk (`wt`)**. Project config is `.config/wt.toml`; its
`post-start` hook runs `direnv allow` on the new tree so the flake shell and
pre-commit hook load on first `cd`.

- Create/switch with the `wt-switch-create` skill, or `wt add <name>` / `wt <name>`.
  Pass `wt --yes` when driving it non-interactively (the project hook prompts for
  approval on first run).
- **Do not use the built-in `EnterWorktree` tool.** It makes a bare `git worktree`
  outside worktrunk's tracking with no `direnv allow`, so the flake shell is absent
  and the pre-commit config is missing (commits then need
  `PRE_COMMIT_ALLOW_NO_CONFIG=1`).
- If your cwd is already a `wt` worktree you are isolated — do not nest another.
- Commit and push the worktree branch; let a human merge to master. When pushing,
  fast-forward — do **not** `git pull --rebase` when master carries merge commits,
  it rewrites history.

## Build, Test & Lint

`just` recipes (run from any subdir):

| Recipe | Runs |
| --- | --- |
| `just build` | `cabal build all` |
| `just test` | `cabal test all` — suites `tests`, `downstream`, `readme` (README.lhs doctest) |
| `just bench` | `cabal bench all` (`benchmark benchmarks`) |
| `just repl [pkg]` | `cabal repl` (default `grid-sized`) |
| `just fmt` | ormolu, in place — same build as the hook |
| `just lint` | `hlint .` (reads `.hlint.yaml` at the root) |
| `just check` | `nix flake check` — hlint + ormolu + build on GHC 9.12 and 9.14 |
| `just watch` | re-run tests on every `.hs` change |
| `just clean` | `cabal clean` |

`nix flake check` builds only the `grid-sized` library and its suites on both
compilers; the example packages are covered by the CI cabal matrix, not by the
flake checks.

## Architecture Overview

- `src/` — the `grid-sized` library, `Data.Grid.Sized.*`: N-dimensional grids with
  every dimension's size fixed at compile time.
  - `Data.Grid.Sized.Internal.Grid.*` — the representation (`Core`, `Shape`,
    `Axis`, `Nest`, `Windows`).
  - `Data.Grid.Sized.Coord.*` — coordinates and per-axis boundary policies:
    `Clamped`, `Periodic` / `Torus`, `Reflective`, `Reflect101`, `Ordinal`. The
    class pair is `IsCoord` (kind `Nat -> Type`) and `IsCoordLifted` (kind `Type`);
    `IsCoordList` is the row-major fold over an axis list. `Coord.hs` re-exports the
    lifted `axis*` wrappers.
  - `Optics.*`, `Stencil`, `Focused`, `Unboxed`, `Unsafe`.
- `tests/` — tasty. `tests-downstream/` and the `readme` doctest are separate suites.
- `bench/` — benchmarks.
- Example apps are listed in `cabal.project` (`sudoko`, `gameOfLife`,
  `ising-example`, `automata`, `maze`, `sokoban`, `grid-atlas`, `atlas-topology`) so
  `cabal build all` covers them; they use gloss — see the `cabal.project` notes on
  the GLUT backend and the stale `containers` bound.
- `spike/` — frozen ADR spikes. Excluded from hlint; do not edit.
- The real downstream consumer is `../aoc`. It shares this `cabal.project` and pins
  GHC 9.12, so keep the default toolchain at 9.12 and let its call sites drive API
  changes.

## Conventions & Patterns

- Formatting is **ormolu**, non-negotiable — the whole tree was reformatted in
  `a597a4b` (listed in `.git-blame-ignore-revs`). Do not hand-format or reformat
  code you did not otherwise touch.
- Nat-indexed class recursion: use a functional dependency, not an associated type
  family — GHC rejects the overlap otherwise.
- Every `IsCoord` method with a `Type`-kinded use gets a one-line lifted `axis*`
  wrapper in `Data.Grid.Sized.Coord.*`, re-exported from `Data.Grid.Sized.Coord`
  (e.g. `axisDistance`, `axisBoundary`, `axisFrameFlips`, `axisOffset`).
- Do not defer a design just because no current consumer needs it.
- Commit subjects end with the beads id in parens: `… (sized-grid-xxxx)`.

## Repomix

`repomix` packs the tree into `repomix-output.xml` (config: `repomix.config.json`)
for feeding the repo to external review tools. It ignores `.beads/`, `ChangeLog.md`,
and golden files; regenerate before sharing.
