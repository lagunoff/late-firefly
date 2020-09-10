{ production ? true }:
let
  nixpkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/dd2727773a23d5aac1f084f0b0891bf5b797199d.tar.gz";
  }) {};

  combine = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {});

  inherit (nixpkgs.pkgs.nix-gitignore) gitignoreSourcePure;
  inherit (nixpkgs) lib;

  filterOutSourceFiles = [
    "*~" "result" ".ghc.*" "dist" "out" "dist-newstyle" ".cabal-sandbox" "tmp" "packages" "*.sqlite" "TAGS" "default.nix"
  ];

  genericLensSrc = builtins.fetchTarball {
    url = "https://github.com/kcsongor/generic-lens/archive/c08a67033777f0fef08aab8820a5a753a30ed961.tar.gz";
  };

  opticsSrc = builtins.fetchTarball {
    url = "https://github.com/well-typed/optics/archive/691f15ff92a71ad227163530f4861acbdce35c6c.tar.gz";
  };

  packages = {
    late-firefly = gitignoreSourcePure filterOutSourceFiles ./.;

    text-show = builtins.fetchTarball {
      url = "https://github.com/RyanGlScott/text-show/archive/da3ed0f7bd3b993611601ec9ed103f7b7322e940.tar.gz";
    };

    tagsoup-lens = ../tagsoup-lens;

    autoexporter = builtins.fetchTarball {
      url = "https://github.com/tfausak/autoexporter/archive/5e4b2e45364e859d553d5090eac19f93fb88c055.tar.gz";
    };

    tagsoup = builtins.fetchTarball {
      url = "https://github.com/lagunoff/tagsoup/archive/c61f55d615350cc2368484baf4608bb39e0b34e8.tar.gz";
    };

    generic-lens = genericLensSrc + "/generic-lens";
    generic-lens-core = genericLensSrc + "/generic-lens-core";
    indexed-profunctors = opticsSrc + "/indexed-profunctors";
  } // lib.optionalAttrs (builtins.pathExists ./locals.nix) (import ./locals.nix);

  lateFireflyExts = hself: []
    ++ lib.optional production (_: attrs: {
      configureFlags = attrs.configureFlags or [] ++ ["-fproduction"];
  });

  haskellPackages = with nixpkgs.haskell.lib;
    nixpkgs.haskell.packages.ghc8102.override {
    overrides = combine [
      (hself: hsuper: nixpkgs.lib.mapAttrs (k: v: hself.callCabal2nix k v {})
        packages)
      (hself: hsuper: nixpkgs.lib.mapAttrs (k: v: v hsuper.${k}) {
        cabal-cargs = x: dontCheck (doJailbreak x);
        text-show = x: dontCheck (doJailbreak x);
        parseargs = x: dontCheck (doJailbreak x);
        late-firefly = x: overrideCabal x (x: combine (lateFireflyExts hself) x x);
      })
    ];
  };
in
  haskellPackages // {
    shell = nixpkgs.mkShell {
      inputsFrom = with haskellPackages; [late-firefly.env];
      buildInputs = with haskellPackages; [cabal-cargs ghcid];
    };
  }
