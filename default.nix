let
  pkgs = import ./nix/pkgs.nix;
in
  pkgs.haskellPackages.my-xmonad
