{ production ? true, minify ? production }:
let
  nixpkgs = import <nixpkgs> {};

  inherit (nixpkgs.pkgs.nix-gitignore) gitignoreSourcePure;
  inherit (nixpkgs) lib;

  combine = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {});

  filterOutSourceFiles = [
    "*~" "result" ".ghc.*" "dist" "out" "dist-newstyle" ".cabal-sandbox" "tmp" "packages" "*.sqlite" "TAGS" "default.nix"
  ];

  packages = {
    late-firefly = gitignoreSourcePure filterOutSourceFiles ./.;

    massaraksh = builtins.fetchTarball {
      url = "https://github.com/lagunoff/massaraksh/archive/5a4877eb0d632325c4eacc3fe20c5bcd8ebdbebe.tar.gz";
    };

    flat = builtins.fetchTarball {
      url = "https://github.com/Quid2/flat/archive/59314709b4b79c1cf6d1084ec4ad88b905d4b5f9.tar.gz";
    };

    text-show = builtins.fetchTarball {
      url = "https://github.com/RyanGlScott/text-show/archive/56c643a05ef8529dab5850949daefd66cf421e44.tar.gz";
    };

    tagsoup-lens = builtins.fetchTarball {
      url = "https://github.com/alpmestan/tagsoup-lens/archive/83ea5b820271f7d72767989b4f5663727229de95.tar.gz";
    };

    tagsoup = builtins.fetchTarball {
      url = "https://github.com/lagunoff/tagsoup/archive/c61f55d615350cc2368484baf4608bb39e0b34e8.tar.gz";
    };
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
  withHoogle = true;
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
