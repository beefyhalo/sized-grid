# Benchmarks

`Main.hs` is a [tasty-bench] suite covering the operations the real workloads
hit. Run it locally with:

```
cabal bench grid-sized:bench:benchmarks
```

The reason it exists, and the reason CI gates on it, is that this library's
performance is not a property of its source text. It is a property of what GHC
does with that text: the wins recorded throughout `IsCoordList` depend on folds
unrolling and methods inlining, and a fold stops unrolling for reasons that are
invisible in a diff -- an `INLINE` dropped, a method turned back into a helper,
a GHC upgrade, a dependency bump. Without numbers, every one of those wins can
be given back silently.

## Two baselines, for two different questions

There are two, and they are not interchangeable.

**`baseline-ghc9.12.3-aarch64-darwin.csv`** is a development reference,
recorded on a maintainer's laptop. Its use is answering "is this change faster
than that change" while you are making the change, on the machine you are
making it on. It is checked in and listed in `extra-source-files` so it travels
with an sdist. CI does not read it, and gating on it would be meaningless: a
GitHub runner is neither that architecture nor that machine, so the honest
threshold would be several hundred percent, which catches nothing.

**The CI baseline** lives in the GitHub Actions cache, not in the tree. Every
green run of the `bench` job on `master` saves its CSV there; every pull
request restores the newest one and compares against it. Both sides of that
comparison come from the same runner class, which is the only way the
percentages mean anything. See the `bench` job in
`.github/workflows/ci.yml` for the thresholds and why they are set where they
are.

## Comparing against a baseline locally

`--baseline` matches by full benchmark name and ignores the extra
`Allocated`/`Copied`/`Peak Memory` columns, so a CSV this suite wrote is
directly usable:

```
cabal bench grid-sized:bench:benchmarks \
  --benchmark-options="--baseline bench/baseline-ghc9.12.3-aarch64-darwin.csv"
```

Add `--fail-if-slower 25` to make it exit non-zero rather than just annotating
each line with a percentage, and `--hide-successes` to print only what moved.
A benchmark that is not in the baseline compares as "same", so adding one never
fails the run.

Note that `--fail-if-faster` is on a different scale from `--fail-if-slower`:
tasty-bench turns it into a lower bound of `1 - percent/100` on the new/old
ratio, so `--fail-if-faster 90` means "fail below 0.1x" and any value `>= 100`
is a bound below zero -- an option that can never fire.

### Confirm a failure before believing it

Do not act on a single run. Measured on this suite, on one machine against its
own baseline, at `--fail-if-slower 25`: with an unrelated build running
alongside, 17 of 48 benchmarks failed at 26-71% slower; the same binary against
the same CSV on the same machine, run again once it was quiet, failed none --
and took 132s rather than 405s, which is the tell.
Contention is indistinguishable from a regression in one sample, and it lands
hardest on the small benchmarks, where `extract 50x50` is 10 ns and a
rescheduled thread is the entire measurement. Re-run on a quiet machine, or
look at whether the failures cluster in time rather than by what they exercise
-- a real regression follows the code, contention follows the clock.

## Regenerating a baseline

**The CI baseline** normally needs no intervention: it is whatever the last
green `master` run measured. It only needs a hand when a slowdown is
*intended*, because a run that fails the gate deliberately does not record a
new baseline -- so an accepted regression would otherwise keep every subsequent
run red. To accept it, run the CI workflow from the Actions tab via
**Run workflow** on `master` with **refresh-baseline** ticked. That records the
current numbers without gating, and leaves a note in the Actions log of who
moved the line and when. Do not reach for it to make a red PR green; a
regression on a branch is a regression, and the baseline moves only on master.

**The laptop baseline** is refreshed by hand, and only deliberately:

```
cabal bench grid-sized:bench:benchmarks \
  --benchmark-options="--csv bench/baseline-ghc9.12.3-aarch64-darwin.csv"
```

Rename the file if the machine is not aarch64-darwin or the compiler is not
9.12.3, and update `extra-source-files` in `grid-sized.cabal` to match --
the architecture and GHC version in the name are load-bearing, because the
numbers are worthless without them. Do it on an idle machine; a build running
in another window is worth tens of percent. Commit the regenerated file on its
own, so the diff is reviewable as a set of numbers rather than buried in the
change that prompted it.

[tasty-bench]: https://hackage.haskell.org/package/tasty-bench
