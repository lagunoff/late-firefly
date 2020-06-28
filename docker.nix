{ pkgs ? import <nixpkgs> {} }:
let
  x = import ./default.nix {};
in with pkgs.dockerTools; buildImage {
  name = "late-firefly-env";
  tag = "latest";
  fromImage = pullImage {
    imageName = "nixos/nix";
    imageDigest = "sha256:2da921898aa6c89e2e60b1fb72d74525b8464b47412482c7f1cf77b8e707a099";
    sha256 = "17bhjbsv2crgngbnklz0wp22lplahahy5547bn209xngmp30izsb";
  };
  contents = x.ghc.late-firefly.env.buildInputs ++ x.ghc.late-firefly.env.nativeBuildInputs ++ [
    pkgs.bashInteractive pkgs.nix pkgs.git
  ];
}
