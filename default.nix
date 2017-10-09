{ mkDerivation, base, containers, filepath, hakyll, pandoc, stdenv
, time
}:
mkDerivation {
  pname = "seba-blog";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers filepath hakyll pandoc time
  ];
  license = stdenv.lib.licenses.unfree;
}
