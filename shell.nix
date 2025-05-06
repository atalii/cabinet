{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    cabal-install ghc zlib pkg-config haskell-language-server cabal2nix
    ormolu

    nodejs
  ];
}
