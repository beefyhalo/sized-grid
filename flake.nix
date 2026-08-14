{
  description = "grid-sized: multidimensional grids with size specified at compile time";

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

        # ghc912 matches what ../aoc builds grid-sized with, so the shared
        # cabal.project between the two repos does not recompile the world.
        # ghc914 is kept building so the move is a one-line change.
        mkPackage = hsPkgs: doCheck (hsPkgs.callCabal2nix "grid-sized" src { });

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

        # The type-checker plugins the library compiles against.
        #
        # nixpkgs' hackage snapshot still has ghc-typelits-natnormalise 0.7.12 /
        # ghc-typelits-knownnat 0.7.13, which sit on ghc-tcplugins-extra 0.5.
        # That package is bounded 'ghc >=7.10 && <9.13' and has no 9.14 release
        # at all, so on GHC 9.14 the old versions cannot even configure
        # ([Cabal-8010]) -- which is the reason sized-grid-h56 sat deferred.
        #
        # natnormalise 0.9 and knownnat 0.8 dropped ghc-tcplugins-extra for
        # ghc-tcplugin-api and widened their bounds past 9.14, so pin all three
        # from Hackage until nixpkgs catches up. Pinned for both compilers rather
        # than only 9.14, so the plugins solve identically on each.
        #
        # dontCheck: the natnormalise test suite shells out to a second GHC to
        # compile fixtures, which does not work inside the sandbox.
        pluginOverrides = hsPkgs:
          hsPkgs.extend (hself: hsuper: {
            ghc-tcplugin-api = hself.callHackageDirect {
              pkg = "ghc-tcplugin-api";
              ver = "0.19.0.0";
              sha256 = "142q8cx6kmn3hzs3542m0yys0kg1bzy75w8rnnpnws36d51vaffs";
            } { };
            ghc-typelits-natnormalise =
              pkgs.haskell.lib.dontCheck (hself.callHackageDirect {
                pkg = "ghc-typelits-natnormalise";
                ver = "0.9.6";
                sha256 = "0yg72pm3sgm47gh7zr3vig29bdfdx3kjpicxarhizb49i15rymkb";
              } { });
            ghc-typelits-knownnat =
              pkgs.haskell.lib.dontCheck (hself.callHackageDirect {
                pkg = "ghc-typelits-knownnat";
                ver = "0.8.4";
                sha256 = "1s49mmdsz2s8836y3zmmsam8khybbbnfyrf3pam6izkwy990q9iz";
              } { });
          });

        ghc912 = pluginOverrides pkgs.haskell.packages.ghc912;
        ghc914 = pluginOverrides pkgs.haskell.packages.ghc914;
      in
      {
        packages = {
          default = mkPackage ghc912;
          grid-sized = mkPackage ghc912;
          grid-sized-ghc914 = mkPackage ghc914;
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
