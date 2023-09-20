with import <nixpkgs> { };
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    ghc
    cabal-install
    glibcLocales
    gnumake
  ];
  shellHook = "export LANG=en_GB.UTF-8";
}

