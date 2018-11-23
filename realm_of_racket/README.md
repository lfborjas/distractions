# Realm of Racket

## Setup

* Install [Racket](https://download.racket-lang.org/) (All of the code here was written using Racket v 7.1).
* Add all the racket binaries to your `PATH`; in my computer, they're located at `/Applications/Racket\ v7.1/bin`

You can run the code by calling `racket file.rkt`; though you'd probably have to edit them to add an invocation to the main function (most have them as a commented-out form at the bottom, e.g. `#(roll-the-dice)`. I worked on these via Emacs with [racket-mode](https://github.com/greghendershott/racket-mode), so I just compiled the file (which also launches the Racket REPL) with `C-c C-k` and then launched the games from the REPL:

![image](https://user-images.githubusercontent.com/82133/48956125-2646d100-ef1f-11e8-802e-30c41d8c6f14.png)



## Complete source code

All the games have been made available by the authors at: https://github.com/racket/realm . Installations of Racket will also include the code, so you can find it in your own filesystem. In my computer, using the default installation location, the source is at `/Applications/Racket v7.1/share/pkgs/realm/`.

## The Games

### Snake

When executed, will begin a new game of snake where "goo" will be generated at random intervals and expire after a random amount of seconds.

![image](https://user-images.githubusercontent.com/82133/48666401-0f236180-ea8f-11e8-86cf-32555695b5d0.png)

### Orc Battle

When executed, you get a turn-based game where you get a random number of attacks per turn and monsters with random health and strength (slimes and brigands can affect not only your health when attacking, but also agility and strength). On each attack, you can either heal yourself, stab a specific monster, or flail wildly (which is weaker, but attacks all monsters at once). When you're out of attacks, all living monsters get to attack you with random damage levels.

![image](https://user-images.githubusercontent.com/82133/48918592-81af8b00-ee5b-11e8-81c1-a7959a1ae6ae.png)

### Dice of Doom

Strategy game for two players, where you can choose to attack a neighboring territory if you have more dice in your attacking territory than your opponent has in the target. For each attack, you navigate with the arrow keys to a source territory, hit enter to mark it, then navigate to an opponent territory and hit enter to mark it; if the attack is possible, it'll be effected and you pay with sending one of your dice to the dice pool. If there's no more attacks to make, you must pass and the computer will replenish all your territories if there's dice to spare. 

There's an `eager` version that tries to compute the entire game tree at the beginning of the game. The board is set to be 2x2, if you change it to 3x3 the computation grows exponentially. The `lazy` version uses [`racket/promise`](https://docs.racket-lang.org/reference/Delayed_Evaluation.html) (`delay` and `force`) to only compute one level of the game tree at a time and create a delayed computation for the next--because of that, one can play on bigger boards (there's a `set-grid` function that can be called before `roll-the-dice` to change the default of 4x4). 

There's also an `ai` version, in which the opposing player is the computer.

![image](https://user-images.githubusercontent.com/82133/48929216-881e2100-eeb4-11e8-8f8c-6416380f8b09.png)


