{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs;
    [
      $0
    ];
  shellHook = ''

  '';
}
