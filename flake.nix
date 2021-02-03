{
  description = "tlater's home configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    # Set up a "dev shell" that will work on all architectures
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let pkgs = inputs.nixpkgs.legacyPackages.${system};
      in { devShell = pkgs.mkShell { buildInputs = with pkgs; [ nixfmt ]; }; });
}
