{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { ipv6 = false; cddl = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "ouroboros-network"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "";
      author = "Alexander Vieth, Marcin Szamotulski, Duncan Coutts";
      homepage = "";
      url = "";
      synopsis = "A networking layer for the Ouroboros blockchain protocol";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.network-mux)
          (hsPkgs.typed-protocols)
          (hsPkgs.typed-protocols-cbor)
          (hsPkgs.io-sim-classes)
          (hsPkgs.contra-tracer)
          (hsPkgs.async)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-binary)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.dns)
          (hsPkgs.fingertree)
          (hsPkgs.iproute)
          (hsPkgs.network)
          (hsPkgs.serialise)
          (hsPkgs.stm)
          (hsPkgs.time)
          (hsPkgs.hashable)
          (hsPkgs.text)
          ];
        };
      exes = {
        "demo-chain-sync" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.contra-tracer)
            (hsPkgs.directory)
            (hsPkgs.network-mux)
            (hsPkgs.network)
            (hsPkgs.ouroboros-network)
            (hsPkgs.QuickCheck)
            (hsPkgs.random)
            (hsPkgs.serialise)
            (hsPkgs.splitmix)
            (hsPkgs.stm)
            (hsPkgs.typed-protocols-cbor)
            (hsPkgs.typed-protocols)
            ];
          };
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.array)
            (hsPkgs.async)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-binary)
            (hsPkgs.cborg)
            (hsPkgs.containers)
            (hsPkgs.contra-tracer)
            (hsPkgs.directory)
            (hsPkgs.dns)
            (hsPkgs.fingertree)
            (hsPkgs.hashable)
            (hsPkgs.io-sim)
            (hsPkgs.io-sim-classes)
            (hsPkgs.iproute)
            (hsPkgs.mtl)
            (hsPkgs.network-mux)
            (hsPkgs.network)
            (hsPkgs.ouroboros-network-testing)
            (hsPkgs.pipes)
            (hsPkgs.process)
            (hsPkgs.QuickCheck)
            (hsPkgs.serialise)
            (hsPkgs.splitmix)
            (hsPkgs.stm)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.tasty)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.typed-protocols-cbor)
            (hsPkgs.typed-protocols)
            ];
          };
        "cddl" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-binary)
            (hsPkgs.cborg)
            (hsPkgs.containers)
            (hsPkgs.fingertree)
            (hsPkgs.hashable)
            (hsPkgs.io-sim-classes)
            (hsPkgs.process-extras)
            (hsPkgs.serialise)
            (hsPkgs.text)
            (hsPkgs.typed-protocols-cbor)
            (hsPkgs.typed-protocols)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "8d51fb7701f8e08724caeb97308c3378a6d30227";
      sha256 = "04blh0k286l9jzdkkgpcswssg114g8hdf49riz5fcpln3zng5nd0";
      });
    postUnpack = "sourceRoot+=/ouroboros-network; echo source root reset to \$sourceRoot";
    }