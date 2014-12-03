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

The function API.WebServer.main runs a websocket server on your localhost where two
players can connect to play the game. A game right now is just a short demo, where
a game is only played for a maximum of five turns. This parameter can be changed in /API/WebServer.hs and will be better when smarter client-server abstraction is built.

The function API.CmdClient.main runs a client to use with the websocket server.
How to play:
* Run the client, and when prompted enter the ip address (or localhost) of the game server.
* Enter your name when prompted.
* The second person to connect goes first.
* If you have any action cards you can play, they will be listed along with your # of action cards you can play, and you will be asked to choose one.
** Future runtime todo: be able to query the contents of a card at any point
** If you enter a card name from the list it will be played, enter anything else to not play a card.
* Then your money and # of buys will be displayed, and all the cards you can buy will be listed.
** Future runtime todo: be able to query the costs of a card at this point
** If you enter a card name from the list it will be bought, enter anything else to not by a card.
* action and buy prompts are not case sensitive, e.g. Chapel == chapel == CHAPEL
* at the end of the game, the game stats will be printed. The most victory points wins.

The game *Dominion* was created by *Donald X. Vaccarino*. Specific card content taken from
www.dominionstrategy.com.
