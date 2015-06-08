# Battleship

An API for simulating a game of "Battleship"; see https://github.com/ambiata/interview/blob/master/battleship.md for the full description. Summarised (in excerpts):

> Maintain the state of a game of "Battleship", including four boards, two for each player, one for recording the current state of the players ships, and one for recording the players' attacks. We want to take care of the basic ability to control the game, and the players moves whilst keeping of track of this game state.

> A player knows:
> * where their own ships are
> * where their own previous attacking moves have been made, and their result
> * where their opponent's previous attacking moves have been made and their result

> Each player should start the game with 5 ships, laid out in non-overlapping positions on their own board:
> Carrier (1x5), Battleship (1x4), Submarine (1x3), Cruiser (1x2), Patrol (1x1)

> The minimal set of operations we want to support:
> * Create an empty board.
> * Place a ship on the board.
> * Create a random board with the ships already placed.
> * Make an attacking move, determining if it is a hit or a miss and updating the game state.
> * Determine the current state of the game, finished (and who won), in play.


## Setup & Development

```
$ git clone git@github.com:damncabbage/haskell-battleship.git battleship
$ cd battleship
$ cabal sandbox init
$ cabal update; cabal install cabal-install
$ cabal install --enable-tests
$ cabal test
```
