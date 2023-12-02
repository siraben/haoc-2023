{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
  my-ghc = haskellPackages.ghcWithPackages (h: [
    h.criterion
    h.vector
    h.hlint
    h.haskell-language-server
  ]);
  my-python = python39.withPackages (ps: with ps; [ numpy ]);
in
mkShell {
  buildInputs = [ my-ghc my-python aoc-cli ];
}
