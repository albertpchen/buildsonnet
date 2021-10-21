with (import <nixpkgs> {});
mkShell rec {
  buildInputs = [
    sbt
    bloop
    dotty
    jsonnet

    zlib
  ];
  LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
