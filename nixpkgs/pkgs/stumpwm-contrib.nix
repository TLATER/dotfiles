{ stdenv, fetchgit }:

stdenv.mkDerivation {
  pname = "stumpwm-contrib";
  version = "2f59028b6558f46510f520b1331ca9fd3f0ccd85";
  src = fetchgit {
    url = "https://github.com/stumpwm/stumpwm-contrib";
    rev = "6514148c7ced697cf949eaaabf8312b9f061f595";
    sha256 = "0gqadd1q8lxr0byrvzx8riivs9q9rfr1ynrk1yc501hnpan6bs8h";
  };
  installPhase = ''
    mkdir -p $out/share/stumpwm/modules
    cp -r * $out/share/stumpwm/modules
  '';
}
