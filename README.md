# NerdLand

This is yet (boring) another attempt to have constructive fun with Haskell.

In this pet project I use a turn-based strategy "with a twist": after waiting 3 seconds for user input, the code gives up and moves on with the game loop. See my other project, <https://github.com/miladhub/rpgfun>, for another pet example demonstrating the two extreme approaches (pure turn or pure concurrent event-based).

The game logic is decoupled from intrastructure, Hexagonal architecture style, by using a Monad that is bound to IO in the real game and to a mocked instance in the tests.

# Running the game

    stack build
    stack exec nerdland-exe

# Developing

  Just open the REPL via `stack ghci` or use <https://github.com/ndmitchell/ghcid>:

    ghcid -c "stack ghci --main-is nerdland:exe:nerdland-exe"

# Testing

  Same, expect now the main belong to the tests:

    stack ghci --main-is nerdland:nerdland-test

    ghcid -c "stack ghci --main-is nerdland:nerdland-test"

# Running the tests

  Easy way:
  
    stack test

  In REPL:

    stack ghci --main-is nerdland:nerdland-test
    ...
    λ> main

    Game
      stop after quit command
      does not log player actions
      logs when npcs become visible
      logs when npcs are out of sight
    Events
      should alert when ogre is in sight

    Finished in 0.0019 seconds
    5 examples, 0 failures

# TODO

* Simplify logic by making it depend on current world only
* Make killing NPCs possible
* Create quests as simple goals made of a `World -> Bool` predicate such as "you killed everyone"
* Define spawn events that add NPCs to the world
* Make random event have effects such as damage to `life` attribute
* Define spawn events that add NPCs to the world
* Make moving switch between areas - e.g., rooms
* Make moving more visual - e.g., draw a grid of the current area
* Add items such as more/less powerful weapons
* Add levels
* Add pre-defined behaviours such as "ogre", "wolf", etc, that determine basic interaction patterns - e.g., attacks, ignores, etc (I'm thinking of using records such as `data Behaviour = { attacks :: Distance -> Bool, ... }`)
* Make quests depend on other quests
* Allow defining quests from a DSL - e.g., parse them from a text file (could use `parsec` for this)
