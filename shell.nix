with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    sbt
    bloop
    dotty
    jsonnet

    zlib
  ];
}
