{ target, daedalus-bridge, ... }:

let
  sources = import ../nix/sources.nix;
  original_daedalus = import sources.daedalus {
    inherit target;
    nodeImplementation = "jormungandr";
    backend = "jormungandr";
    cluster = "staging";
    version = "wallet-test";
    buildNum = "4321";
  };
  daedalus = original_daedalus.overrideScope' (self: super: {
    inherit daedalus-bridge;
  });
in {
  x86_64-windows = daedalus.windows-installer;
  x86_64-linux = daedalus.newBundle;
}
