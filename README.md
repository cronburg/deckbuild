deckbuild-model
===============

Reference the commands in `buildme` on how to build / install this package. This package
relies on our deckbuild-language package which contains the parser and quasiquoter for
our Haskell EDSL, meaning you need to do `cabal add-source ...` the language package
before you can build / run this package.

The function Examples.First.test currently plays a deckbuilding game for the
quasiquote-defined cards, thus the output is the end-result of a game between
two *greedy* AI / computer players. We hope to in the coming weeks create a command
line interface to allow humans to play against each other, or against an AI.

The game *Dominion* was created by *Donald X. Vaccarino*. Specific card content taken from
www.dominionstrategy.com.
