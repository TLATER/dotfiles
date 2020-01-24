{ pkgs }:

with pkgs;

stdenv.mkDerivation {
  pname = "oh-my-zsh-emacs";
  version = "4e45e12dc355e3ba34e7e40ce4936fb222f0155c";
  src = fetchFromGitHub {
    owner = "ohmyzsh";
    repo = "ohmyzsh";
    rev = "4e45e12dc355e3ba34e7e40ce4936fb222f0155c";
    sha256 = "0i8fd9wjgj6hcf5ykwyfim93ydvlnclqa9cga3w0fal87dgr261w";
  };
  installPhase = ''
    mkdir -p $out/
    install tools/require_tool.sh $out/
    install tools/require_tool.sh $out/
  '';
}
