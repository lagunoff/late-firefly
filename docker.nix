{ pkgs ? import <nixpkgs> {} }:
let
  x = import ./default.nix {};
in pkgs.dockerTools.buildImage {
  name = "late-firefly-env";
  tag = "latest";
  contents = x.ghc.late-firefly.env.buildInputs ++ x.ghc.late-firefly.env.nativeBuildInputs ++ [
    pkgs.bashInteractive pkgs.nix
  ];
}
