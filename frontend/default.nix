{
  pkgs ? import <nixpkgs> { },
  rev,
}:

pkgs.buildNpmPackage (finalAttrs: {
  pname = "cabinet-frontend";
  version = "0.1.0";
  src = ./.;

  npmDepsHash = "sha256-Vqp78iXwyO/Rp1V8sLnPuhMPQYbpxvU6lCVcr8CkKFE=";

  # We rely on git to read the version at build-time.
  nativeBuildInputs = with pkgs; [ git ];

  installPhase = ''
    mkdir -p $out/lib
    cp -r build $out/lib/cabinet-frontend
  '';

  CABINET_FRONTEND_VERSION = rev;
})
