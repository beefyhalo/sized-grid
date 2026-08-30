#!/usr/bin/env bash
# PostToolUse hook: after Claude edits a Haskell source file, check it with
# ormolu and hlint and hand any findings straight back to Claude (exit 2) so
# they are fixed in the same turn rather than surfacing later in CI.
#
# Wired up in .claude/settings.json for Edit|Write|MultiEdit. Same tools and
# same .hlint.yaml as the git pre-commit hook and CI (see flake.nix's
# `preCommit`); this is only the fast, per-file feedback loop.
#
# Silent (exit 0) when it has nothing to say, when the edited file is not a
# linted .hs file, or when ormolu/hlint are not on PATH (outside `nix develop`).

set -euo pipefail

input=$(cat)
file=$(printf '%s' "$input" | jq -r '.tool_input.file_path // empty')
[ -n "$file" ] || exit 0

case "$file" in
  *.hs) ;;
  *) exit 0 ;;
esac
# Frozen ADR spikes are out of lint scope (matches .hlint.yaml / the hook).
case "$file" in
  */spike/*|*/dist-*|*/.direnv/*) exit 0 ;;
esac
[ -f "$file" ] || exit 0

command -v ormolu >/dev/null 2>&1 || exit 0
command -v hlint  >/dev/null 2>&1 || exit 0

out=""
if ! diff=$(ormolu --mode check "$file" 2>&1); then
  out+="ormolu: $file is not formatted. Run: ormolu -i \"$file\""$'\n'
fi
if ! hints=$(hlint "$file" 2>&1); then
  out+="$hints"$'\n'
fi

[ -n "$out" ] || exit 0

printf '%s' "$out" >&2
exit 2
