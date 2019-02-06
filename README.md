# reversi

You must have `stack` installed to play this game.

If you don't have `stack` already go [here](https://docs.haskellstack.org/en/stable/README/) to learn how to install it.

1. Clone the repository 

2. Cd into `reversi`

3. Run the command `stack ghci`

4. Once everything is compiled, type `main` into the prompt to start a two-player game or type `randomGame` to generate a random game.

The rules of Reversi can be found [here](https://en.wikipedia.org/wiki/Reversi).

When prompted enter a location in the format `(x, y)`. e.g. `(2,1)`

A list of valid locations is provided for each player, every turn.
If the input is invalid you will prompted to try again.

Continue to play until both players don't have anymore moves.

To play again, type `main` into the prompt.

Use `Ctrl + C` to exit the game at any time.

You can replay the previous game state by entering `((-1),(-1))`.

A.I. Coming Soon
