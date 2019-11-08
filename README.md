# NerdLand

This is yet (boring) another attempt to have constructive fun with Haskell.

In this pet project I use a turn-based strategy "with a twist": after waiting 3 seconds for user input, the code gives up and moves on with the game loop. See my other project, <https://github.com/miladhub/rpgfun>, for another pet example demonstrating the two extreme approaches (pure turn or pure concurrent event-based).

The game logic is decoupled from intrastructure, Hexagonal architecture style, by using a Monad that is bound to IO in the real game and to a mocked instance in the tests.

# Running the game

    stack build
    stack exec nerdland-exe

# Developing

    ghcid -c "stack ghci --main-is nerdland:exe:nerdland-exe"
