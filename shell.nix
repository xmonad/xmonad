let
    project = import ./release.nix;
    pkgs = project.pkgs;

    inherit (pkgs) haskellPackages;
in
    pkgs.stdenv.mkDerivation {
      name = "shell";
      buildInputs = project.app.env.nativeBuildInputs ++ [
        haskellPackages.cabal-install
      ];
    }
