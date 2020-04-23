{ production ? true }:
let
  nixpkgsFunc = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs-channels/archive/9d55c1430af72ace3a479d5e0a90451108e774b4.tar.gz";
  });

  nixpkgs = nixpkgsFunc {};

  inherit (nixpkgs.pkgs.nix-gitignore) gitignoreSourcePure;
  inherit (nixpkgs) lib;

  foldExtensions = lib.foldl' lib.composeExtensions (self: super: {});

  customOverridesPre = helf: huper: with nixpkgs.haskell.lib;  {
    mkDerivation = args:
      huper.mkDerivation (args // {
        doCheck = false;
        doHaddock = false;
      });
  };

  filterOutSourceFiles = [
    "*~" "result" ".ghc.*" "dist" "dist-newstyle" ".cabal-sandbox" "tmp" "packages" "*.sqlite" "TAGS" "default.nix"
  ];

  noop = _: _: {};

  customOverridesPost = hself: hsuper: let
    inherit (nixpkgs.haskell.lib) dontCheck doJailbreak overrideCabal;

    flat = hself.callCabal2nix "flat" (builtins.fetchTarball {
      url = "https://github.com/Quid2/flat/archive/936e0d8d6d510058cbd70b22d82e8bf2ba41c9dc.tar.gz";
    }) {};

    text-show = doJailbreak (hself.callCabal2nix "text-show" (builtins.fetchTarball {
      url = "https://github.com/RyanGlScott/text-show/archive/56c643a05ef8529dab5850949daefd66cf421e44.tar.gz";
    }) {});

    # flat-rpc = hsuper.callCabal2nix "flat-rpc" (builtins.fetchGit {
    #   url = "git@github.com:lagunoff/flat-rpc.git";
    #   rev = "66758ee467118228c477bae4f6172d23a609e2f2";
    # }) {};

    flat-rpc = hsuper.callCabal2nix "flat-rpc" (gitignoreSourcePure filterOutSourceFiles ../flat-rpc) {};

    monadXhrSrc = gitignoreSourcePure filterOutSourceFiles ../monad-xhr;

    # monad-xhr = hsuper.callCabal2nix "monad-xhr" (builtins.fetchGit {
    #   url = "git@github.com:lagunoff/monad-xhr.git";
    #   rev = "35a4e6adee9475a275cfd163a0e5ad475744b61d";
    # }) {};

    monad-xhr = hsuper.callCabal2nix "monad-xhr" monadXhrSrc {};

    tagsoup-lens = hsuper.callCabal2nix "tagsoup-lens" (builtins.fetchGit {
      url = "git@github.com:alpmestan/tagsoup-lens.git";
      rev = "83ea5b820271f7d72767989b4f5663727229de95";
    }) {};

    tagsoup = hsuper.callCabal2nix "tagsoup" (builtins.fetchGit {
      url = "git@github.com:lagunoff/tagsoup.git";
      rev = "c61f55d615350cc2368484baf4608bb39e0b34e8";
    }) {};

    massaraksh = hsuper.callCabal2nix "massaraksh" (builtins.fetchTarball {
      url = "https://github.com/lagunoff/massaraksh/archive/6d8d386cadd59f8ce899b5f1e5e55ba721532ae8.tar.gz";
    }) {};

    lateFireflyExts = []
      ++ lib.optional production (_: attrs: {
        configureFlags = attrs.configureFlags or [] ++ ["-fproduction"];
      })
      ++ lib.optional ((hself.ghc.isGhcjs or false) && production) (_: attrs: {
        postInstall = (attrs.postInstall or "") + ''
          ${nixpkgs.closurecompiler}/bin/closure-compiler\
            --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars\
            --externs=$out/bin/late-firefly.jsexe/all.js.externs\
            --externs=${monadXhrSrc}/src-ghcjs/Control/Monad/Xhr/Internal.js.externs\
            $out/bin/late-firefly.jsexe/all.js > $out/bin/late-firefly.jsexe/all.min.js
          rm $out/bin/late-firefly.jsexe/all.js
          mv $out/bin/late-firefly.jsexe/all.min.js $out/bin/late-firefly.jsexe/all.js
        '';
      })
      ++ lib.optional (hself.ghc.isGhcjs or false) (_: attrs: {
        postInstall = (attrs.postInstall or "") + ''
          rm $out/bin/late-firefly
          cp $src/assets/index.html $out/bin/late-firefly.jsexe/index.html
          find $out/bin/late-firefly.jsexe/ -type f ! -regex '.*\(index.html\|all.js\)' -delete
          ${nixpkgs.zopfli}/bin/zopfli -i1000 $out/bin/late-firefly.jsexe/all.js
        '';
      });
  in {
    inherit flat text-show flat-rpc monad-xhr tagsoup-lens tagsoup massaraksh;
    aeson = doJailbreak hsuper.aeson;
    bloodhound = doJailbreak hsuper.bloodhound;
    hjsonpointer = doJailbreak hsuper.hjsonpointer;
    hjsonschema = doJailbreak hsuper.hjsonschema;
    transformers-lift = doJailbreak hsuper.transformers-lift;
    late-firefly = overrideCabal (hsuper.callCabal2nix "late-firefly" (gitignoreSourcePure filterOutSourceFiles ./.) {}) (x: foldExtensions lateFireflyExts x x);
  };

  haskellOverlaysPre = [customOverridesPre];

  haskellOverlaysPost = [customOverridesPost];

  reflex-platform-src = builtins.fetchTarball {
    url = "https://github.com/reflex-frp/reflex-platform/archive/1f04194d051c3a4fd54aa30d4c46a5659261a620.tar.gz";
  };

  reflex-platform = import reflex-platform-src {
    inherit config nixpkgsFunc haskellOverlaysPre haskellOverlaysPost;
  };

  config = { allowBroken = true; };

in reflex-platform.project({ pkgs, ... }: {
  useWarp = true;
  withHoogle = false;
  packages = {};

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
