let
  pkgsv = import ( import ./nixpkgs.nix );
  pkgs = pkgsv {};
  smosPkgs =
    pkgsv {
      overlays =
        [
          ( import ./gitignore-src.nix )
          ( import ./overlay.nix )
        ];
    };
in
  smosPkgs
