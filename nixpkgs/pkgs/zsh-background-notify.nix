{ pkgs }:

with pkgs;

stdenv.mkDerivation {
  pname = "zsh-background-notify";
  version = "d5f0430cb052f82c433c17707816910da87e201e";
  src = fetchFromGitHub {
    owner = "t413";
    repo = "zsh-background-notify";
    rev = "d5f0430cb052f82c433c17707816910da87e201e";
    sha256 = "0p8fk50bxr8kg2v72afg7f2n09n9ap0yn7gz1i78nd54l0wc041n";
  };
  propagatedBuildInputs = [ libnotify ];
  installPhase = ''
    mkdir -p $out/
    install bgnotify.plugin.zsh $out/
  '';
}
