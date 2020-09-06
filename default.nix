{ production ? true, minify ? production }:
let
  nixpkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/db5bbef31fa05b9634fa6ea9a5afbea463da88ea.tar.gz";
  }) {};

  inherit (nixpkgs.pkgs.nix-gitignore) gitignoreSourcePure;
  inherit (nixpkgs) lib;

  combine = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {});

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
      url = "https://github.com/RyanGlScott/text-show/archive/56c643a05ef8529dab5850949daefd66cf421e44.tar.gz";
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

  overrides = pkgs: with pkgs.haskell.lib; let
    ovr = x: dontCheck (doJailbreak (dontHaddock x));
    lateFireflyExts = hself: []
      ++ lib.optional production (_: attrs: {
        configureFlags = attrs.configureFlags or [] ++ ["-fproduction"];
      })
      ++ lib.optional ((hself.ghc.isGhcjs or false) && minify) (_: attrs: {
        postInstall = (attrs.postInstall or "") + ''
          ${pkgs.closurecompiler}/bin/closure-compiler\
            --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars\
            --externs=$out/bin/late-firefly.jsexe/all.js.externs\
            $out/bin/late-firefly.jsexe/all.js > $out/bin/late-firefly.jsexe/all.min.js
          rm $out/bin/late-firefly.jsexe/all.js
          mv $out/bin/late-firefly.jsexe/all.min.js $out/bin/late-firefly.jsexe/all.js
        '';
      })
      ++ lib.optional ((hself.ghc.isGhcjs or false) && minify) (_: attrs: {
        postInstall = (attrs.postInstall or "") + ''
          rm $out/bin/late-firefly
          cp $src/assets/index.html $out/bin/late-firefly.jsexe/index.html
          find $out/bin/late-firefly.jsexe/ -type f ! -regex '.*\(index.html\|all.js\)' -delete
          ${pkgs.zopfli}/bin/zopfli -i1000 $out/bin/late-firefly.jsexe/all.js
        '';
      });
  in combine [
    (hself: hsuper: nixpkgs.lib.mapAttrs (k: v: ovr (hself.callCabal2nix k v {}))
      packages)

    (hself: hsuper: nixpkgs.lib.mapAttrs (k: v: v hsuper.${k}) {
      cabal-cargs = ovr;
      aeson = ovr;
      bloodhound = ovr;
      hjsonpointer = ovr;
      hjsonschema = ovr;
      transformers-lift = ovr;
      late-firefly = x: overrideCabal x (x: combine (lateFireflyExts hself) x x);
    })
  ];

  reflex-platform = import (builtins.fetchTarball {
    url = "https://github.com/reflex-frp/reflex-platform/archive/846964a0895e819946c1a415202aee414d27cfa3.tar.gz";
  }) { config.allowBroken = true; };

in reflex-platform.project({ pkgs, ... }: {
  useWarp = true;
  withHoogle = false;
  packages = {};

  shells = {
    ghc = ["late-firefly"];
    ghcjs = ["late-firefly"];
  };

  shellToolOverrides = ghc: super: {
    haskell-ide-engine = null;
    hdevtools = null;
    hlint = null;
    stylish-haskell = null;
    cabal-cargs = pkgs.haskell.lib.dontCheck pkgs.haskellPackages.cabal-cargs;
  };

  overrides = overrides pkgs;
})
