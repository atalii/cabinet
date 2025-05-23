{
  mkDerivation,
  aeson,
  base,
  bytestring,
  containers,
  HTTP,
  http-types,
  HUnit,
  lib,
  scotty,
  stm,
  text,
  time,
  uuid,
  wai-extra,
  lens,
}:
mkDerivation {
  pname = "cabinet";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    bytestring
    containers
    stm
    text
    time
    uuid
    lens
  ];
  executableHaskellDepends = [
    base
    aeson
    bytestring
    HTTP
    http-types
    scotty
    stm
    text
    uuid
    wai-extra
  ];
  testHaskellDepends = [
    base
    bytestring
    HUnit
  ];
  license = lib.licenses.mpl20;
  mainProgram = "cabinet-srv";
}
