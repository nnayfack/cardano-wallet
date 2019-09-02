{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
# Import IOHK common nix lib
, iohkLib ? import ./nix/iohk-common.nix { inherit system crossSystem config; }
# Use nixpkgs pin from iohkLib
, pkgs ? iohkLib.pkgs
}:

with import ./nix/util.nix { inherit pkgs; };

let
  haskell = iohkLib.nix-tools.haskell { inherit pkgs; };
  src = iohkLib.cleanSourceHaskell ./.;

  inherit (iohkLib.rust-packages.pkgs) jormungandr;
  cardano-sl-node = import ./nix/cardano-sl-node.nix { inherit pkgs; };

  haskellPackages = import ./nix/default.nix {
    inherit pkgs haskell src;
    inherit cardano-sl-node jormungandr;
    inherit (iohkLib.nix-tools) iohk-extras iohk-module;
  };

in {
  inherit pkgs iohkLib src haskellPackages;
  inherit cardano-sl-node jormungandr;
  inherit (haskellPackages.cardano-wallet.identifier) version;

  inherit (haskellPackages.cardano-wallet.components.exes)
    cardano-wallet-jormungandr;

  tests = collectComponents "tests" isCardanoWallet haskellPackages;
  benchmarks = collectComponents "benchmarks" isCardanoWallet haskellPackages;

  shell = haskellPackages.shellFor {
    name = "cardano-wallet-shell";
    packages = ps: with ps; [
      cardano-wallet
      cardano-wallet-cli
      cardano-wallet-launcher
      cardano-wallet-core
      cardano-wallet-core-integration
      cardano-wallet-jormungandr
      cardano-wallet-shelley
      cardano-wallet-test-utils
      bech32
      text-class
    ];
    buildInputs =
      with pkgs.haskellPackages; [ hlint stylish-haskell weeder ghcid ]
      ++ [ jormungandr cardano-sl-node pkgs.pkgconfig pkgs.sqlite-interactive ];
  };

  checks.check-nix-tools = pkgs.callPackage ./nix/check-nix-tools.nix {};
}
