{ pkgs }:

with pkgs;

stdenv.mkDerivation {
  pname = "stumpwm-contrib";
  version = "2f59028b6558f46510f520b1331ca9fd3f0ccd85";
  src = fetchgit {
    url = "https://github.com/stumpwm/stumpwm-contrib";
    rev = "2f59028b6558f46510f520b1331ca9fd3f0ccd85";
    sha256 = "1f1chwqyac8ndbb917rz3kl3ngiyx68np0xgjcqncbxfs1l48k38";
  };
  installPhase = ''
    mkdir -p $out/share/stumpwm/modules
    cp -r * $out/share/stumpwm/modules
  '';
}
