# This file is maintained by @IvanMalison (github)
{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    git-ignore-nix.url = github:IvanMalison/gitignore.nix/master;
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix }:
  let
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
        (hself: hsuper: {
          xmonad = hself.callCabal2nix "xmonad" (git-ignore-nix.gitIgnoreSource ./.) { };
        });
      });
    };
    overlays = [ overlay ];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; };
  in
  rec {
    devShell = pkgs.haskellPackages.shellFor {
      packages = p: [ p.xmonad ];
    };
    defaultPackage = pkgs.haskellPackages.xmonad;
  }) // { inherit overlay overlays; } ;
}
