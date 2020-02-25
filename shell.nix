{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let
  inherit (lib) makeLibraryPath;
  hs = haskell.packages.ghc865;
  tools = [
    (hs.ghcWithPackages (ps: [ps.GLUT ps.OpenGL ps.OpenGLRaw]))
    hs.cabal-install
    hs.ghcid
    pkgs.binutils-unwrapped
  ];
  libraries = [
    
  ];
  libraryPath = "${makeLibraryPath libraries}";
in
  pkgs.runCommand "shell" {
    buildInputs = tools ++ libraries;
    shellHook = ''
      export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${libraryPath}"
      export LIBRARY_PATH="${libraryPath}"
    '';
  } ""
