{ pkgs ? import <nixpkgs> {} }:

pkgs.buildNpmPackage (finalAttrs: {
  pname = "cabinet-frontend";
  version = "0.1.0";
  src = ./.;

  npmDepsHash = "sha256-iOSY3GfKCcpzl8wICF0qcGSKxvGOdXF/VtNrAthTiQw=";

  installPhase = ''
    mkdir -p $out/lib
    cp -r build $out/lib/cabinet-frontend
  '';
})
