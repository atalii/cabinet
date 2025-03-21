{ mkDerivation, base, blaze-html, blaze-markup, bytestring
, containers, file-embed, http-types, HUnit, lib, scotty, stm, text
, time, uuid, wai-extra
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
    base blaze-html blaze-markup bytestring file-embed http-types
    scotty stm text uuid wai-extra
  ];
  testHaskellDepends = [ base bytestring HUnit ];
  license = lib.licenses.mpl20;
  mainProgram = "cabinet-srv";
}
