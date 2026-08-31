{
  description = "grid-sized: multidimensional grids with size specified at compile time";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    # hlint + ormolu as a git pre-commit hook and a `nix flake check`, both
    # from one config -- see `preCommit` below. `follows` so the hook tools
    # (ormolu especially) are the same build as the devShell's, not a second
    # nixpkgs' version that would reformat differently.
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, git-hooks, ... }:
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
          # Installs the git pre-commit hook on first `nix develop`; a no-op
          # afterwards. enabledPackages puts hlint/ormolu on PATH so `hlint .`
          # and `ormolu` in the shell are the exact builds the hook and CI use.
          inherit (preCommit) shellHook;
          nativeBuildInputs = [
            hsPkgs.haskell-language-server
            hsPkgs.cabal-install
            pkgs.pkg-config
            pkgs.zlib
            # `just repomix` -- packs the tree for external review tools.
            pkgs.repomix
          ] ++ preCommit.enabledPackages;
        };

        # hlint + ormolu, run three ways from one definition: the git
        # pre-commit hook (via mkShell's shellHook), `nix flake check` /
        # `nix build .#checks.<system>.preCommit` in CI (ci.yml's lint job),
        # and `nix develop -c pre-commit run --all-files` by hand.
        #
        # hlint reads .hlint.yaml at the repo root (scope excludes, the
        # deliberate-lambda ignores); spike/** is dropped here too so the hook
        # never lints the frozen ADR spikes even when one is touched. ormolu
        # covers every .hs file -- the tree was reformatted wholesale in
        # a597a4b (see .git-blame-ignore-revs).
        preCommit = git-hooks.lib.${system}.run {
          src = ./.;
          excludes = [ "^dist-" "^\\.direnv/" "^result" ];
          hooks = {
            hlint = {
              enable = true;
              package = pkgs.hlint;
              excludes = [ "^spike/" ];
            };
            ormolu = {
              enable = true;
              package = pkgs.ormolu;
            };
          };
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

        # GHC 9.14.1 ships template-haskell 2.24, and nixpkgs' snapshot of
        # constraints-extras 0.4.0.2 -- pulled in by dependent-sum, which
        # finitary needs -- still caps it at <2.24, so the package cannot even
        # configure ([Cabal-8010]) and takes grid-sized down with it. The bound
        # is stale rather than a real incompatibility: 0.4.0.2 compiles against
        # template-haskell 2.24, which is what the cabal 9.14.1 matrix job does
        # via a Hackage revision nixpkgs has not picked up. Scoped to 9.14 so
        # 9.12 keeps hitting the binary cache unchanged, and to this one package
        # so an unrelated future bound still surfaces as a build error.
        ghc914Overrides = hsPkgs:
          hsPkgs.extend (hself: hsuper: {
            constraints-extras =
              pkgs.haskell.lib.doJailbreak hsuper.constraints-extras;
          });

        ghc912 = pluginOverrides pkgs.haskell.packages.ghc912;
        ghc914 = ghc914Overrides (pluginOverrides pkgs.haskell.packages.ghc914);
      in
      {
        packages = {
          default = mkPackage ghc912;
          grid-sized = mkPackage ghc912;
          grid-sized-ghc914 = mkPackage ghc914;
        };

        # `nix flake check` builds grid-sized and runs *its* suites (tests,
        # downstream, readme) on both compilers. Only grid-sized:
        # callCabal2nix reads the root grid-sized.cabal, and nothing in the
        # library depends on atlas-topology or grid-atlas, so neither is in
        # this build graph at all. Their suites are run by ci.yml's cabal
        # matrix job instead (sized-grid-svil) -- do not read these checks as
        # covering the whole cabal.project.
        checks = {
          ghc912 = mkPackage ghc912;
          ghc914 = mkPackage ghc914;
          preCommit = preCommit;
        };

        devShells = {
          default = mkShell ghc912;
          ghc914 = mkShell ghc914;
        };
      });
}
