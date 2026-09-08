---
name: dispatch
description: Supervise bead work across tiered agents — triage `bd ready` into tiers, hand mechanical issues to a cheaper worker in its own worktree, wait for it, run the quality gate, then land or reject. Use when asked to dispatch, delegate, hand off, or work through the ready queue; also when asked to triage or tier beads without dispatching. Do not use for working an issue yourself.
---

# Dispatch

You are the supervisor. You triage, you verify, and you merge. Workers write code
in isolated worktrees and never touch master.

`$ARGUMENTS` selects the phase:

| Invocation | Does |
| --- | --- |
| `/dispatch` | Triage the ready queue, propose a routing table, stop for approval |
| `/dispatch <bead-id>` | Dispatch that one bead to its labelled tier |
| `/dispatch reap` | Check running workers, gate and land whatever finished |

Run `bd prime` first if beads context is not already loaded.

## Tiers

Route by **type-level content, not by apparent diff size.** A three-line change to
a coordinate instance is supervisor work; a 300-line CI refactor is not.

| Label | Worker | Takes |
| --- | --- | --- |
| `agent:supervisor` | you, in this session | Anything touching `Data.Grid.Sized.Coord.*`, `Internal.Grid.*`, the shape algebra, `IsCoord`/`IsCoordLifted`/`IsCoordList` instances, Nat-indexed class recursion, optics, or a design/ADR call. Also all `type=decision` and any bead whose description asks a question rather than naming a change. |
| `agent:sonnet` | `wt-bead <id> -- --model sonnet` | CI and workflow files, benchmark harness plumbing, demos and example apps, Haddock and docs, metadata, test scaffolding — work whose correctness a human can check by reading the diff and running the gate. |

There is no local-model tier. Both Ollama models were measured and produce
nothing usable here; see the bd memory `local-ollama-models-are-not-usable-as-bead`
before proposing one again.

When a bead looks mechanical but its *verification* needs type-level judgement,
it is supervisor work. The gate is what you can check, not what the worker can write.

## Phase 1 — triage

```bash
bd ready --json | jq -r '.[] | [.id, .priority, .title] | @tsv'
bd show <id>            # read description AND design/notes before labelling
```

Read each candidate properly. The `design` and `notes` fields routinely contain a
recorded decision that changes the tier — a bead titled like a chore often carries
a design note that makes it a judgement call.

Label, do not dispatch yet:

```bash
bd label add <id> agent:sonnet
bd list --label-any agent:sonnet --status=open --json | jq -r '.[] | [.id,.title] | @tsv'
```

Then **show the routing table and stop.** Tiering is cheap to get wrong and
expensive to discover wrong, so a human confirms the first pass. Say which beads
you deliberately kept for yourself and why.

## Phase 2 — dispatch

One worker per bead. Check nothing is already running on it:

```bash
herdr agent list | jq -r '.result.agents[] | "\(.agent) \(.agent_status) \(.pane_id) \(.cwd)"'
```

Then hand it off. `wt-bead` claims the bead, creates the worktree and branch,
opens a herdr workspace, starts the agent and types the prompt:

```bash
wt-bead <id> -- --model sonnet          # prompt typed, NOT submitted — a human hits Enter
wt-bead <id> -y -- --model sonnet       # submitted; unattended
```

Default to the form without `-y` unless the user asked for unattended dispatch.
The unsent prompt is the last cheap moment to catch a bad hand-off.

`WT_CLAUDE_KIND` picks the agent binary (default `claude`); everything after `--`
goes to that binary. Do not use `EnterWorktree`, and do not create worktrees with
raw `git worktree` — see CLAUDE.md.

## Phase 3 — join

```bash
herdr agent list | jq -r '.result.agents[] | select(.cwd|test("<branch>")) | .pane_id'
herdr agent wait <pane> --until idle --until done --until blocked --timeout 1800000
```

Run the wait in the background so you stay responsive; you are notified when it
returns. `blocked` means the worker is asking for input — read the pane with
`herdr agent read <pane>` and decide whether to answer it or reject the attempt.

## Phase 4 — the gate

**Never trust a worker's own claim that it is done.** Run the gate yourself, in
the worktree, before looking at anything else:

```bash
wt=$(wt list --format=json | jq -r --arg b "<branch>" '
  (if type=="array" then .[] else (.items // .worktrees // [])[] end)
  | select((.branch // .worktree.branch) == $b)
  | (.path // .worktree.path // empty)' | head -1)

direnv exec "$wt" just test
direnv exec "$wt" just lint
git -C "$wt" diff master...HEAD
```

`direnv exec` matters — the flake devshell supplies GHC, cabal and the hooks, and
`cabal` outside it fails in ways that look like the worker's fault.

Then read the diff against the bead's actual ask. Reject work that passes the gate
but does something other than what the bead described, quietly widens scope, or
edits `spike/` (frozen ADR spikes — do not edit).

## Phase 5 — land or reject

**Land** — fast-forward only, and the supervisor does it, never the worker:

```bash
cd "$wt" && wt merge --no-squash --no-rebase
bd close <id>
```

`--no-squash` keeps the individual commits, `--no-rebase` is not optional: master
carries merge commits and rebasing rewrites history. Never `git pull --rebase` on
master, and never force-push. Commit subjects end with the bead id in parens.

**Reject** — leave no trace and record why:

```bash
bd update <id> --status=open --assignee=""
bd update <id> --notes="<what the worker produced and why it was rejected>"
git -C "$wt" log --oneline master..HEAD    # confirm what would be lost
wt remove --foreground <branch>
```

Check that log before removing. If the worker produced something worth keeping,
say so and stop rather than deleting it.

Record a durable finding with `bd remember` when a worker fails in a way that
should change future routing — not for a one-off bad run.

## Invariants

- A worker never merges, never pushes, and never touches master.
- The gate runs in the worktree, under `direnv exec`, before the diff review.
- One agent per pane; `wt-claude` focuses an existing agent rather than stacking a second.
- Beads state transitions belong to the supervisor. Give workers `bd --readonly` if they only need to read.
- `bd`, not TodoWrite or markdown lists. `bd remember`, not MEMORY.md.
