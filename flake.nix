# This file is maintained by @IvanMalison (github)
{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    git-ignore-nix.url = github:hercules-ci/gitignore.nix/master;
    unstable.url = github:NixOS/nixpkgs/nixos-unstable;
  };
  outputs = { self, flake-utils, nixpkgs, unstable, git-ignore-nix }:
  let
    fromHOL = hol: final: prev: with prev.lib; with attrsets;
      setAttrByPath [ "haskellPackages" ]
        ((getAttrFromPath [ "haskellPackages" ] prev).override (old: {
          overrides = composeExtensions (old.overrides or (_: _: {}))
            (hol final prev);
        }));
    patch = unstable
          + "/pkgs/development/haskell-modules/patches/xmonad_0_17_0-nix.patch";
    hoverlay = final: prev: hself: hsuper:
      with prev.haskell.lib.compose; {
      xmonad = appendPatch patch
        (hself.callCabal2nix "xmonad"
          (git-ignore-nix.lib.gitignoreSource ./.) { });
      };
    overlay = fromHOL hoverlay;
    overlays = [ overlay ];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; };
  in
  rec {
    devShell = pkgs.haskellPackages.shellFor {
      packages = p: [ p.xmonad ];
    };
    defaultPackage = pkgs.haskellPackages.xmonad;
  }) // { inherit overlay overlays; lib = { inherit fromHOL; }; };
}
