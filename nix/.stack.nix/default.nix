{
  extras = hackage:
    {
      packages = {
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "OddWord" = (((hackage.OddWord)."1.0.2.0").revisions).default;
        "quickcheck-state-machine" = (((hackage.quickcheck-state-machine)."0.6.0").revisions).default;
        "command" = (((hackage.command)."0.1.1").revisions).default;
        "clock" = (((hackage.clock)."0.8").revisions).default;
        "prometheus" = (((hackage.prometheus)."2.1.2").revisions).default;
        "time-units" = (((hackage.time-units)."1.0.0").revisions).default;
        "libsystemd-journal" = (((hackage.libsystemd-journal)."1.4.4").revisions).default;
        "katip" = (((hackage.katip)."0.8.3.0").revisions).default;
        bech32 = ./bech32.nix;
        cardano-wallet-core = ./cardano-wallet-core.nix;
        cardano-wallet-core-integration = ./cardano-wallet-core-integration.nix;
        cardano-wallet-cli = ./cardano-wallet-cli.nix;
        cardano-wallet-launcher = ./cardano-wallet-launcher.nix;
        text-class = ./text-class.nix;
        cardano-wallet-test-utils = ./cardano-wallet-test-utils.nix;
        cardano-wallet-jormungandr = ./cardano-wallet-jormungandr.nix;
        zip = ./zip.nix;
        persistent = ./persistent.nix;
        persistent-sqlite = ./persistent-sqlite.nix;
        persistent-template = ./persistent-template.nix;
        cardano-crypto = ./cardano-crypto.nix;
        contra-tracer = ./contra-tracer.nix;
        iohk-monitoring = ./iohk-monitoring.nix;
        lobemo-backend-aggregation = ./lobemo-backend-aggregation.nix;
        lobemo-backend-monitoring = ./lobemo-backend-monitoring.nix;
        ekg-prometheus-adapter = ./ekg-prometheus-adapter.nix;
        };
      };
  resolver = "lts-13.24";
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "zip" = { flags = { "disable-bzip2" = lib.mkOverride 900 true; }; };
          };
        })
    {
      packages = {
        "$locals" = { package = { ghcOptions = "-ddump-to-file -ddump-hi"; }; };
        };
      }
    ];
  }