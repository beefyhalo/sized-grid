# Revision history for sokoban

## 0.1.0.0 -- unreleased

* The rules, on a Mobius strip: move, push, undo and win through
  `Data.Grid.Atlas.Mobius.mobiusStep`, with the push written as two steps in
  the frame each one lands in.
* Levels as pictures of themselves, in the traditional Sokoban character set,
  at a size read out of the picture and reified into the board's type.
* Breadth-first search over game states, so a level that ships is one that has
  been finished.
