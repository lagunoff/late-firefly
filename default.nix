{}:
let
  fetchTarball = uri: builtins.fetchTarball uri;

  pkgs = import <nixpkgs> {};

  nixpkgsFunc = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs-channels/archive/9d55c1430af72ace3a479d5e0a90451108e774b4.tar.gz";
  });

  nixpkgs = nixpkgsFunc {};

  customOverridesPre = helf: huper: with nixpkgs.haskell.lib;  {
    mkDerivation = args:
      huper.mkDerivation (args // {
        doCheck = false;
        doHaddock = false;
      });
  };

  customOverridesPost = hself: hsuper: let
    inherit (nixpkgs.haskell.lib) dontCheck doJailbreak overrideCabal;
  in {
    aeson = doJailbreak hsuper.aeson;
    bloodhound = doJailbreak hsuper.bloodhound;
    hjsonpointer = doJailbreak hsuper.hjsonpointer;
    hjsonschema = doJailbreak hsuper.hjsonschema;
    transformers-lift = doJailbreak hsuper.transformers-lift;
    flat = hself.callCabal2nix "flat" (builtins.fetchTarball {
      url = "https://github.com/Quid2/flat/archive/936e0d8d6d510058cbd70b22d82e8bf2ba41c9dc.tar.gz";
    }) {};
    text-show = doJailbreak (hself.callCabal2nix "text-show" (builtins.fetchTarball {
      url = "https://github.com/RyanGlScott/text-show/archive/56c643a05ef8529dab5850949daefd66cf421e44.tar.gz";
    }) {});
    monad-rpc = hsuper.callCabal2nix "monad-rpc" ../monad-rpc {};
    flat-rpc = hsuper.callCabal2nix "flat-rpc" ../flat-rpc {};
    monad-xhr = hsuper.callCabal2nix "monad-xhr" ../monad-xhr {};
    massaraksh = hsuper.callCabal2nix "massaraksh" ../massaraksh {};
    tagsoup-lens = hsuper.callCabal2nix "tagsoup-lens" ./packages/tagsoup-lens {};
    late-firefly = overrideCabal (hsuper.callCabal2nixWithOptions "late-firefly" ./. "-fproduction" {}) (_: {
      postInstall = ''
        ${pkgs.closurecompiler}/bin/closure-compiler --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars --externs=$out/bin/infinite-sea.jsexe/all.js.externs $out/bin/infinite-sea.jsexe/all.js > $out/bin/infinite-sea.jsexe/all.min.js
      '';
    });
  };

  haskellOverlaysPre = [
    customOverridesPre
  ];

  haskellOverlaysPost = [
    customOverridesPost
  ];

  config = { allowBroken = true; };

  reflex-platform-src = builtins.fetchTarball {
    url = "https://github.com/reflex-frp/reflex-platform/archive/1f04194d051c3a4fd54aa30d4c46a5659261a620.tar.gz";
  };

  reflex-platform = import reflex-platform-src {
    inherit config nixpkgsFunc haskellOverlaysPre haskellOverlaysPost;
  };
in reflex-platform.project({ pkgs, ... }: {
  useWarp = true;
  withHoogle = false;

  packages = {
  };

  shells = {
    ghc = ["late-firefly"];
    ghcjs = ["late-firefly"];
  };

  shellToolOverrides = ghc: super: {
    closure-compiler = null;
    haskell-ide-engine = null;
    hdevtools = null;
    hlint = null;
    stylish-haskell = null;
  };
})
