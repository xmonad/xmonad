# This file is maintained by @IvanMalison (github)
{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    git-ignore-nix.url = github:hercules-ci/gitignore.nix/master;
    unstable.url = github:NixOS/nixpkgs/nixos-unstable;
  };
  outputs = { self, flake-utils, nixpkgs, unstable, git-ignore-nix }:
  let
    hpath = { prefix ? null, compiler ? null }:
      (if prefix == null then [] else [ prefix ]) ++
      (if compiler == null
       then [ "haskellPackages" ]
       else [ "haskell" "packages" compiler ]
      );
    fromHOL = hol: comp: final: prev: with prev.lib; with attrsets;
      setAttrByPath (hpath comp)
        ((getAttrFromPath (hpath comp) prev).override (old: {
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
    overlay = fromHOL hoverlay { };
    overlays = [ overlay ];
    nixosModule = { config, pkgs, lib, ... }: with lib; with attrsets;
      let
        cfg = config.services.xserver.windowManager.xmonad.flake;
        comp = { inherit (cfg) prefix compiler; };
      in {
        options = {
          services.xserver.windowManager.xmonad.flake = with types; {
            enable = mkEnableOption "flake";
            prefix = mkOption {
              default = null;
              type = nullOr string;
              example = literalExpression "\"unstable\"";
              description = ''
                Specify a nested alternative <literal>pkgs</literal> by attrName.
              '';
            };
            compiler = mkOption {
              default = null;
              type = nullOr string;
              example = literalExpression "\"ghc922\"";
              description = ''
                Which compiler to build xmonad with.
                Must be an attribute of <literal>pkgs.haskell.packages</literal>.
                Sets <option>xmonad.haskellPackages</option> to match.
              '';
            };
          };
        };
        config = mkIf cfg.enable {
          nixpkgs.overlays = [ (fromHOL hoverlay comp) ];
          services.xserver.windowManager.xmonad.haskellPackages =
            getAttrFromPath (hpath comp) pkgs;
        };
      };
    nixosModules = [ nixosModule ];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; };
  in
  rec {
    devShell = pkgs.haskellPackages.shellFor {
      packages = p: [ p.xmonad ];
    };
    defaultPackage = pkgs.haskellPackages.xmonad;
  }) // {
    inherit hoverlay overlay overlays nixosModule nixosModules;
    lib = { inherit hpath fromHOL; };
  };
}
