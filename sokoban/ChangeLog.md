# Revision history for sokoban

## 0.1.0.0 -- unreleased

* The rules, on a Mobius strip: move, push, undo and win through
  `Data.Grid.Atlas.Mobius.mobiusStep`, with the push written as two steps in
  the frame each one lands in.
* Levels as pictures of themselves, in the traditional Sokoban character set,
  at a size read out of the picture and reified into the board's type.
* Breadth-first search over game states, so a level that ships is one that has
  been finished.
* A window, with two views of the surface: flat, with the far side of each edge
  drawn past it and coloured tabs pairing the rows the seam joins; and
  player-centred, drawn in the player's own frame.
* `sokoban-shot`, which photographs the game's own window through
  `glReadPixels`, because this machine cannot photograph it from outside.
* Eight levels in a ramp by idea, each one saying what it teaches.
* `Sokoban.Flat`, which plays the same layout on a cylinder and on a plain
  rectangle of the same shape, so that "this level needs the half turn" is a
  fact the test suite checks rather than a claim the level note makes.
