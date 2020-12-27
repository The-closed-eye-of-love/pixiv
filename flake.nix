{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;

  outputs = { self, nixpkgs, flake-utils, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in
          with pkgs;
          {
            devShell = pixiv-dev.envFunc { withHoogle = true; };
            defaultPackage = pixiv;
          }
    ) // {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          pixiv = hpkgs.callCabal2nix "pixiv" ./. {};
        in
          with super; with haskell.lib;
          {
            inherit pixiv;
            pixiv-dev = addBuildTools pixiv [
              haskell-language-server
              cabal-install
            ];
          };
    };
}
