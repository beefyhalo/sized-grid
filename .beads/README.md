# Beads in this repo

Issue tracking is [beads](https://github.com/steveyegge/beads) (`bd`). This file
used to be upstream's generic README; it was replaced because two of its claims
are false here — this project has no Dolt remote to `bd dolt push` to, and it
does not auto-sync on commit (auto-export is deliberately off).

**Read `CLAUDE.md` → "Beads: How This Repo Is Wired" before touching anything in
`.beads/`.** It covers the embedded engine, why there is no `dolt sql-server`,
how worktrees share this directory, and the session-end export.

Day to day:

```bash
bd ready                # find available work
bd show <id>            # issue detail
bd update <id> --claim  # claim
bd close <id>           # complete
bd prime                # full command reference
```

At session end:

```bash
bd export -o .beads/issues.jsonl && sort -o .beads/issues.jsonl .beads/issues.jsonl
bd backup sync
```

The sort is load-bearing — `bd export` emits memory rows in non-deterministic
order, so an unsorted export of an unchanged database churns ~80 lines.
