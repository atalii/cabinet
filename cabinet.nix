{ mkDerivation, base, blaze-html, blaze-markup, bytestring
, containers, HTTP, http-types, HUnit, lib, scotty, stm, text
, time, uuid, wai-extra, utf8-string
}:
mkDerivation {
  pname = "cabinet";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers stm text time uuid
  ];
  executableHaskellDepends = [
    base blaze-html blaze-markup bytestring HTTP http-types
    scotty stm text uuid wai-extra utf8-string
  ];
  testHaskellDepends = [ base bytestring HUnit ];
  license = lib.licenses.mpl20;
  mainProgram = "cabinet-srv";
}
