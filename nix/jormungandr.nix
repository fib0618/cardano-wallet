############################################################################
# Packages for jormungandr and jcli.
#
# These use the functions from iohk-nix to build the version that we
# require for cardano-wallet.
#
# Linux and macOS versions are built from source, and so can be any revision.
# The Windows version comes from the binary releases, and so the
# versions are limited to the binary releases provided.
#
# To change the version:
#
# 1. Adjust the "version" and/or "rev" variables below.
#
# 2. Then invert the first digit in *all* sha256 hashes.
#    That is, change 0 to 1 and 1 to 0. It's important that you change
#    them to something different than before, otherwise you may get
#    the previous version from your local cache. So change:
#     - release.sha256       --  -source
#     - release.cargoSha256  --  -vendor
#     - windows.sha256       --  .zip
#
# 3. Keep running "nix-build nix/jormungandr.nix" (or let CI do it for you)
#    until there are no errors about "hash mismatch in fixed-output derivation".
#    Update the corresponding sha256 entry each time.
#
# 4. If you now run "nix-shell" you should have updated versions of
#    jormungandr and jcli.
#
############################################################################

{ iohkLib ? import ./iohk-common.nix {}
, pkgs ? iohkLib.pkgs
}:

let
  release = rec {
    version = "0.8.0-rc3";
    # Git revision of input-output-hk/jormungandr repo.
    rev = "v${version}";
    # Hash of git repo and all of its submodules.
    sha256 = "0csqd60hz9vixvyp1rracz18lvjkspsm9r0df7sxs5inp1f3x8mk";
    # Hash of all Cargo dependencies.
    cargoSha256 = "14v7v9rl04yajwxh7qcpzjnc3swmpdbmjqw7arnms8cbdfbqc9q6";
  };

  windows = rec {
    # URL and hash of windows binary release
    url = "https://github.com/input-output-hk/jormungandr/releases/download/v${release.version}/jormungandr-v${release.version}-x86_64-pc-windows-msvc.zip";
    sha256 = "1s66y8n39cxz2yk2ihxvl3apxcyxryjc4pgzz16nfpisf7bkvkb4";
  };

  jormungandr-win64 = pkgs.runCommand "jormungandr-win64-${release.version}" {
    nativeBuildInputs = [ pkgs.buildPackages.unzip ];
  } ''
    mkdir -p $out/bin
    cd $out/bin
    unzip ${pkgs.fetchurl windows}
  '';

  nonWindows = pkg: if pkgs.stdenv.hostPlatform.isWindows
    then jormungandr-win64
    else pkg;

in rec {
  jormungandr = nonWindows (iohkLib.rust-packages.pkgs.makeJormungandr release);
  jormungandr-cli = nonWindows (iohkLib.rust-packages.pkgs.makeJcli release);

  inherit jormungandr-win64;
  inherit (jormungandr) src;
}
