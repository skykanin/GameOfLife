# GameOfLife
[Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) implementation in Haskell with GUI.

# Dev mode
Clone the repository then `cd` to the folder, run `nix-shell` then run `cabal run exe:gameoflife`. If you're not using the nix stable channel you will have to override
the `pkgs` argument using `nix-shell --arg pkgs "import <stable> {}"` where `stable` is the name of the stable channel.
