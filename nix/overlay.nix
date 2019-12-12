final:
  previous:
    with final.haskell.lib;
      let xmonadPackage =
            let pathForXmonad = final.gitignoreSource ../my-xmonad;
            in disableLibraryProfiling (final.haskellPackages.callCabal2nix "xmonad" pathForXmonad {});
      in {
      haskellPackages = previous.haskellPackages.override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (
        self: super:
          {
            my-xmonad = xmonadPackage;
          }
        );
      });
    }
