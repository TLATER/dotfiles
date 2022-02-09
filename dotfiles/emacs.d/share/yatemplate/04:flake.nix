{
  description = "$1";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-$2";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    let inherit (flake-utils.lib) eachSystem defaultSystems;
    in {

    } // (eachSystem defaultSystems) (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs;
            [
              $0
            ];
          shellHook = ''

          '';
        };
      });
}
