with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "cabal-env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [ zlib ];
}
