{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation {
  pname = "oh-my-zsh-screen";
  version = "4e45e12dc355e3ba34e7e40ce4936fb222f0155c";
  src = fetchFromGitHub {
    owner = "ohmyzsh";
    repo = "ohmyzsh";
    rev = "05e2956dc61198d4767b96d97c5d10c93cedd6e3";
    sha256 = "1fw48vjksxii9m5chrlsabc3zf0fq9bgn2z487f23gd1vlpplj6l";
  };
  installPhase = ''
    mkdir -p $out/
    install plugins/screen/screen.plugin.zsh $out/
  '';
}
