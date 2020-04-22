let
  nixpkgs = import <nixpkgs> {};
  project = import ./default.nix { production = true; };
  server = project.ghc.late-firefly;
  client = project.ghcjs.late-firefly;
  start = nixpkgs.writeShellScriptBin "late-firefly-start"
    ''
      ${server}/bin/late-firefly start --docroot ${client}/bin/late-firefly.jsexe --port 8080 "$@"
    '';
in { inherit server client start; }

