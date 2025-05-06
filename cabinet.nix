{ mkDerivation, base, blaze-html, blaze-markup, bytestring
, containers, HTTP, http-types, HUnit, lib, scotty_0_22, stm, text
, time, uuid, wai-extra
}:
mkDerivation {
  pname = "cabinet";
  version = "0.1.0.0";
  src = ./backend;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers stm text time uuid
  ];
  executableHaskellDepends = [
    base blaze-html blaze-markup bytestring HTTP http-types
    scotty_0_22 stm text uuid wai-extra
  ];
  testHaskellDepends = [ base bytestring HUnit ];
  license = lib.licenses.mpl20;
  mainProgram = "cabinet-srv";
}
