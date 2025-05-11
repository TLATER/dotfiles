# Bump nushell-dbus to 0.13 since the upstream package is no longer
# compatible with the current upstream nushell version.
#
# This *should* be fixed upstream, but NixOS 25.05 is right around the
# corner, so I'm not going to bother with the PR process.
{ fetchFromGitHub, nushellPlugins }:
nushellPlugins.dbus.overrideAttrs (
  old:
  let
    version = "0.13.0";
    src = fetchFromGitHub {
      owner = "devyn";
      repo = old.pname;
      rev = version;
      hash = "sha256-w+0H0A+wQa4BUzKx9G2isn29IicoZsLlWCDnC3YSzek=";
    };
  in
  {
    inherit src;
    version = "0.13.0";
    cargoDeps = old.cargoDeps.overrideAttrs (_: {
      inherit src;
      name = "${old.pname}-vendor.tar.gz";
      outputHash = "sha256-wRVNodfY7Q410V4YPLPWn2rsPc612X/MtJ5Jy4zs/zA=";
    });
  }
)
