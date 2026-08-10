{
  description = "sized-grid: multidimensional grids with size specified at compile time";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs.haskell.lib) doCheck;

        # Keep build inputs minimal so editing build artefacts does not
        # invalidate the derivation.
        src = pkgs.lib.cleanSourceWith {
          src = ./.;
          filter = path: type:
            let base = baseNameOf (toString path);
            in !(builtins.elem base [
              ".stack-work"
              "dist-newstyle"
              ".direnv"
              ".beads"
              "result"
            ]) && pkgs.lib.cleanSourceFilter path type;
        };

        # ghc912 matches what ../aoc builds sized-grid with, so the shared
        # cabal.project between the two repos does not recompile the world.
        # ghc914 is kept building so the move is a one-line change.
        mkPackage = hsPkgs: doCheck (hsPkgs.callCabal2nix "sized-grid" src { });

        mkShell = hsPkgs: hsPkgs.shellFor {
          packages = p: [ (mkPackage hsPkgs) ];
          # withHoogle builds haddocks for every dependency; far too slow to pay
          # on each `nix develop`.
          withHoogle = false;
          nativeBuildInputs = [
            hsPkgs.haskell-language-server
            hsPkgs.cabal-install
            pkgs.pkg-config
            pkgs.zlib
          ];
        };

        ghc912 = pkgs.haskell.packages.ghc912;
        ghc914 = pkgs.haskell.packages.ghc914;
      in
      {
        packages = {
          default = mkPackage ghc912;
          sized-grid = mkPackage ghc912;
          sized-grid-ghc914 = mkPackage ghc914;
        };

        # `nix flake check` builds the library and runs the tasty suite on both.
        checks = {
          ghc912 = mkPackage ghc912;
          ghc914 = mkPackage ghc914;
        };

        devShells = {
          default = mkShell ghc912;
          ghc914 = mkShell ghc914;
        };
      });
}
