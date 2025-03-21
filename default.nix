{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callPackage (import ./cabinet.nix) {}
