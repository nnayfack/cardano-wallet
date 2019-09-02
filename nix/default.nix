{ pkgs

# haskell.nix
, haskell

# Filtered sources of this project
, src

# Test dependencies of cardano-wallet
, cardano-sl-node
, jormungandr

# Customisations for cross-compiling
, iohk-extras ? {}
, iohk-module ? {}

}:

let
  # our packages
  stack-pkgs = import ./.stack.nix/default.nix;

  # Grab the compiler name from stack-to-nix output.
  compiler = (stack-pkgs.extras {}).compiler.nix-name;

  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    modules = [
      # Add source filtering to local packages
      {
        packages.cardano-wallet.src = src;
        packages.cardano-wallet-core.src = src + /lib/core;
        packages.cardano-wallet-core-integration.src = src + /lib/core-integration;
        packages.cardano-wallet-cli.src = src + /lib/cli;
        packages.cardano-wallet-launcher.src = src + /lib/launcher;
        packages.cardano-wallet-jormungandr.src = src + /lib/jormungandr;
        packages.cardano-wallet-shelley.src = src + /lib/shelley;
        packages.cardano-wallet-test-utils.src = src + /lib/test-utils;
        packages.text-class.src = src + /lib/text-class;
        packages.bech32.src = src + /lib/bech32;
      }

      # Add dependencies
      {
        packages.cardano-wallet-jormungandr.components.tests = {
          integration.build-tools = [ jormungandr ];
          unit.build-tools = [ jormungandr ];
        };
      }

      # Misc. build fixes for dependencies
      {
        # Cut down iohk-monitoring deps
        packages.iohk-monitoring.flags = {
          disable-ekg = true;
          disable-examples = true;
          disable-graylog = true;
          disable-gui = true;
          disable-prometheus = true;
          disable-systemd = true;
        };

        # Katip has Win32 (>=2.3 && <2.6) constraint
        packages.katip.doExactConfig = true;
      }

      # the iohk-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.  For now we need to
      # list the packages that require template haskell
      # explicity here.
      iohk-module
    ];
    pkg-def-extras = [
      iohk-extras.${compiler}
      (hackage: { packages = {
        "transformers" = (((hackage.transformers)."0.5.6.2").revisions).default;
        "process" = (((hackage.process)."1.6.5.0").revisions).default;
      }; })
    ];
  };

in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
