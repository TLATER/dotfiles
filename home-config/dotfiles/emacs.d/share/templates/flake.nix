{
  description = "$1";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-$2";
  };

  outputs = {nixpkgs, ...}: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    devShells.${system}.default = {
      packages = with pkgs; [
        $0
      ];
    };
  };
}
