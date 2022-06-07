let
  pkgs = import <nixpkgs> {};
  x86_pkgs = import <nixpkgs> { system = "x86_64-darwin"; };
in
  pkgs.mkShell rec {
    buildInputs = [
      pkgs.sbt
      pkgs.dotty
      pkgs.jsonnet
      pkgs.openjdk

      pkgs.zlib
      pkgs.sqlite
      pkgs.coursier
      x86_pkgs.bloop
    ] ++ (pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.darwin.apple_sdk.frameworks.Foundation);
    LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;

    shellHook = ''
      alias coursier=cs
    '';
  }
