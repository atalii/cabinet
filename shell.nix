{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [ cabal-install ghc zlib haskell-language-server ];
}
