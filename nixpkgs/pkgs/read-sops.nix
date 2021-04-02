{ stdenv, python3, sops }:

stdenv.mkDerivation {
  pname = "read-sops";
  version = "1.0";
  src = ../../dotfiles/bin;
  installPhase = ''
    mkdir -p $out/bin
    install read-sops $out/bin
  '';
  buildInputs =
    [ (python3.withPackages (pypkgs: with pypkgs; [ xdg ruamel_yaml ])) ];
  propagatedBuildInputs = [ sops ];
}
