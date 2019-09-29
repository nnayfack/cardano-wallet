{ pkgs

# haskell.nix
, haskell

# Filtered sources of this project
, src

# Test dependencies of cardano-wallet
, cardano-sl-node
, cardano-http-bridge
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

  # Use a postInstall wrapping script if this is not a windows
  # build. Otherwise, copy DLL dependencies.
  wrapForPosix = postInstall: if pkgs.stdenv.hostPlatform.isWindows
    then ''
      cp -v ${pkgs.libffi}/bin/libffi-6.dll $out/bin
    '' else postInstall;

  # Chop out a subdirectory of the source, so that the package is only
  # rebuilt when something in the subdirectory changes.
  filterSubDir = dir:  with pkgs.lib; let
      isFiltered = src ? _isLibCleanSourceWith;
      origSrc = if isFiltered then src.origSrc else src;
    in cleanSourceWith {
      inherit src;
      filter = path: type:
        type == "directory" ||
        hasPrefix (toString origSrc + toString dir) path;
    } + dir;

  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    modules = [
      # Add source filtering to local packages
      {
        packages.cardano-wallet-core.src = filterSubDir /lib/core;
        packages.cardano-wallet-core-integration.src = filterSubDir /lib/core-integration;
        packages.cardano-wallet-cli.src = filterSubDir /lib/cli;
        packages.cardano-wallet-launcher.src = filterSubDir /lib/launcher;
        packages.cardano-wallet-http-bridge.src = filterSubDir /lib/http-bridge;
        packages.cardano-wallet-jormungandr.src = filterSubDir /lib/jormungandr;
        packages.cardano-wallet-test-utils.src = filterSubDir /lib/test-utils;
        packages.text-class.src = filterSubDir /lib/text-class;
        packages.bech32.src = filterSubDir /lib/bech32;
      }

      # Add dependencies
      {
        packages.cardano-wallet-http-bridge.components.tests = {
          integration.build-tools = [ cardano-http-bridge cardano-sl-node ];
          unit.build-tools = [ cardano-http-bridge ];
        };

        # fixme: better way of setting environment variables
        packages.cardano-wallet-http-bridge.preBuild = "export NETWORK=testnet";

        packages.cardano-wallet-jormungandr.components.tests = {
          integration.build-tools = [ jormungandr ];
          unit.build-tools = [ jormungandr ];
        };

        packages.cardano-wallet-jormungandr.components.exes.cardano-wallet-jormungandr = {
          build-tools = [ pkgs.makeWrapper];
          postInstall = wrapForPosix ''
            wrapProgram $out/bin/cardano-wallet-jormungandr \
              --prefix PATH : ${jormungandr}/bin
          '';
        };

        packages.cardano-wallet-http-bridge.components.exes.cardano-wallet-http-bridge = {
          build-tools = [ pkgs.makeWrapper];
          postInstall = wrapForPosix ''
            wrapProgram $out/bin/cardano-wallet-http-bridge \
              --prefix PATH : ${cardano-http-bridge}/bin
          '';
        };

        packages.cardano-wallet-http-bridge.components.benchmarks.restore = {
          build-tools = [ pkgs.makeWrapper ];
          postInstall = ''
            makeWrapper \
              $out/cardano-wallet-*/restore \
              $out/bin/restore \
              --prefix PATH : ${cardano-http-bridge}/bin
          '';
        };

        # Workaround for Haskell.nix issue
        packages.cardano-wallet-jormungandr.components.all.postInstall = pkgs.lib.mkForce "";
        packages.cardano-wallet-http-bridge.components.all.postInstall = pkgs.lib.mkForce "";
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
