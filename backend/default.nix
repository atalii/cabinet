{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callPackage (import ./cabal-package.nix) {}
