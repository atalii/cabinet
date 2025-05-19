{ pkgs ? import <nixpkgs> {} }:

pkgs.buildNpmPackage (finalAttrs: {
  pname = "cabinet-frontend";
  version = "0.1.0";
  src = ./.;

  npmDepsHash = "sha256-Vqp78iXwyO/Rp1V8sLnPuhMPQYbpxvU6lCVcr8CkKFE=";

  installPhase = ''
    mkdir -p $out/lib
    cp -r build $out/lib/cabinet-frontend
  '';
})
