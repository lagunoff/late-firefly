(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    telikov = ./telikov;
    massaraksh = ./massaraksh;
    haste-app = ./haste-app;
    sqlite-simple-stub = ./sqlite-simple-stub;
    direct-sqlite-stub = ./direct-sqlite-stub;
  };

  shells = {
    ghcjs = ["telikov"];
  };
})
