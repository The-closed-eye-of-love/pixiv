{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
      in with pkgs; {
        devShells.default = pixiv-dev.envFunc { withHoogle = true; };
        packages.default = pixiv;
      }) // {
        overlays.default = final: prev:
          let
            hpkgs = prev.haskellPackages;
            linkHaddockToHackage = drv:
              prev.haskell.lib.overrideCabal drv (drv: {
                haddockFlags = [
                  "--html-location='https://hackage.haskell.org/package/$pkg-$version/docs'"
                ];
              });
            pixiv = with prev.haskell.lib;
              linkHaddockToHackage (disableLibraryProfiling
                (dontCheck (hpkgs.callCabal2nix "pixiv" ./. { })));
          in with prev;
          with haskell.lib; {
            inherit pixiv;
            pixiv-dev =
              addBuildTools pixiv [ haskell-language-server cabal-install ];
          };
      };
}
