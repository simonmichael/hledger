final: prev: {

  # First, call stacklock2nix and pass it the `stack.yaml` for your project.
  hledger-stacklock = final.stacklock2nix {
    stackYaml = ../stack.yaml;

    # When using `stacklock2nix`, you may need to specify a newer all-cabal-hashes.
    #
    # This is necessary when you are using a Stackage snapshot/resolver or
    # `extraDeps` in your `stack.yaml` file that is _newer_ than the
    # `all-cabal-hashes` derivation from the Nixpkgs you are using.
    #
    # If you are using the latest nixpkgs-unstable and an old Stackage
    # resolver, then it is usually not necessary to override
    # `all-cabal-hashes`.
    #
    # If you are using a very recent Stackage resolver and an old Nixpkgs,
    # it is almost always necessary to override `all-cabal-hashes`.
    #
    # WARNING: If you're on a case-insensitive filesystem (like some OSX
    # filesystems), you may get a hash mismatch when using fetchFromGitHub
    # to fetch all-cabal-hashes.  As a workaround in that case, you may
    # want to use fetchurl:
    #
    # ```
    # all-cabal-hashes = final.fetchurl {
    #   url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/f3f41d1f11f40be4a0eb6d9fcc3fe5ff62c0f840.tar.gz";
    #   sha256 = "sha256-vYFfZ77fOcOQpAef6VGXlAZBzTe3rjBSS2dDWQQSPUw=";
    # };
    # ```
    #
    # You can find more information in:
    # https://github.com/NixOS/nixpkgs/issues/39308
    all-cabal-hashes = final.fetchFromGitHub {
      owner = "commercialhaskell";
      repo = "all-cabal-hashes";
      rev = "0ed4e913d9b6b8cb2e66d71dcfc5c53f96232a6d";
      sha256 = "sha256-O0VAO0ov3ytTvaqfahFfQwc4ewG1Nejk+m4f3zwlg14=";
    };
  };

  # Then, apply the Haskell package overlays provided by stacklock2nix to the
  # Haskell package set you want to use.
  #
  # This gives you a normal Haskell package set with packages defined by your
  # stack.yaml and Stackage snapshot / resolver.
  hledger-pkg-set =
    final.haskell.packages.ghc945.override (oldAttrs: {

      # Make sure the package set is created with the same all-cabal-hashes you
      # passed to `stacklock2nix`.
      inherit (final.hledger-stacklock) all-cabal-hashes;

      overrides = final.lib.composeManyExtensions [
        # Make sure not to lose any old overrides, although in most cases there
        # won't be any.
        (oldAttrs.overrides or (_: _: {}))

        # An overlay with Haskell packages from the Stackage snapshot.
        final.hledger-stacklock.stackYamlResolverOverlay

        # An overlay with `extraDeps` from `stack.yaml`.
        final.hledger-stacklock.stackYamlExtraDepsOverlay

        # An overlay with your local packages from `stack.yaml`.
        final.hledger-stacklock.stackYamlLocalPkgsOverlay

        # Suggested overrides for common problems.
        final.hledger-stacklock.suggestedOverlay

        # Any additional overrides you may want to add.
        (hfinal: hprev: {
          # Infinite recursion fixes
          foldable1-classes-compat = final.haskell.lib.dontCheck hprev.foldable1-classes-compat;
          hjsmin = final.haskell.lib.dontCheck hprev.hjsmin;
          # ???
          haskeline = final.haskell.lib.dontCheck hprev.haskeline_0_8_2_1;
          # Missing dep
          persistent-sqlite = final.haskell.lib.addBuildDepend hprev.persistent-sqlite final.sqlite;
          # Patch for wrong operator usage
          sqlite = final.haskell.lib.overrideCabal (final.haskell.lib.appendPatches
                    hprev.sqlite
                    [(final.fetchpatch {
                      url = "https://gist.githubusercontent.com/silky/ee599d8ac705593bfa690b15770c0274/raw/c28d43d65e424655a5c9417d19e1b577785b451c/sqlite.patch";
                      sha256 = "sha256-4oxOe5iBC8Ah/eCuL56H48l+uqhI2JPyLUQsW9H1GME=";
                    })])
                    # Note broken anymore
                    (_: { broken = false; });
          # Patch for old Setup.hs style
          csv = final.haskell.lib.appendPatches hprev.csv
                    [(final.fetchpatch {
                      url = "https://gist.githubusercontent.com/silky/4454c2a16ac9099fe54fd7932cb5a7ed/raw/36350b35e3e152636c98efafbdfc23e43020c249/csv.patch";
                      sha256 = "sha256-zlzksu74FJuh3fMo8g8Y5kNhXAShXMXkp+aEoaugIZg=";
                    })];
        })
      ];
    });

  # Finally, you can pull out the Haskell package you're interested in and
  # build it with Nix.  This will normally be one of your local packages.
  hledger = final.hledger-pkg-set.hledger;

  # You can also easily create a development shell for hacking on your local
  # packages with `cabal`.
  hledger-dev-shell =
    final.hledger-pkg-set.shellFor {
      packages = haskPkgs: final.hledger-stacklock.localPkgsSelector haskPkgs;
      # Additional packages that should be available for development.
      nativeBuildInputs = [
        # Some Haskell tools (like cabal-install) can be taken from the
        # top-level of Nixpkgs.
        final.cabal-install
        # final.ghcid
        final.hpack
        final.stack
        # Some Haskell tools need to have been compiled with the same compiler
        # you used to define your stacklock2nix Haskell package set.  Be
        # careful not to pull these packages from your stacklock2nix Haskell
        # package set, since transitive dependency versions may have been
        # carefully setup in Nixpkgs so that the tool will compile, and your
        # stacklock2nix Haskell package set will likely contain different
        # versions.
        # final.haskell.packages.ghc945.haskell-language-server
        # Other Haskell tools may need to be taken from the stacklock2nix
        # Haskell package set, and compiled with the example same dependency
        # versions your project depends on.
      ];
    };
}
