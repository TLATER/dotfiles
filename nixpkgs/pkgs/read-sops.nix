{ pkgs }:

pkgs.stdenv.mkDerivation {
  pname = "read-sops";
  version = "1.0";
  src = ../../dotfiles/bin;
  installPhase = ''
    mkdir -p $out/bin
    install read-sops $out/bin
  '';
  buildInputs =
    [ (pkgs.python3.withPackages (pypkgs: with pypkgs; [ xdg ruamel_yaml ])) ];
  propagatedBuildInputs = with pkgs; [ sops ];
}
