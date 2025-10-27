{
  description = "tlater's dotfiles";

  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-25.11/nixexprs.tar.xz";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-gaming = {
      url = "github:fufexan/nix-gaming";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-webapps.url = "github:TLATER/nix-webapps?ref=tlater/idiomatic-flake";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };

    tree-sitter-sieve = {
      url = "github:TLATER/tree-sitter-sieve";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flint = {
      url = "github:NotAShelf/flint";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-flatpak.url = "github:gmodena/nix-flatpak/latest";
    famedly-nixos = {
      url = "github:TLATER/famedly-nixos/tlater/nixos-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self, nixpkgs, ... }@inputs:
    {
      nixosConfigurations = {
        yui = nixpkgs.lib.nixosSystem {
          modules = [
            ./nixos-config
            ./nixos-config/hosts/yui
          ];

          specialArgs.flake-inputs = inputs;
        };

        rin = nixpkgs.lib.nixosSystem {
          modules = [
            ./nixos-config
            ./nixos-config/hosts/rin
          ];

          specialArgs.flake-inputs = inputs;
        };
      };

      nixosModules.nvidia = ./nixos-modules/nvidia;

      packages.x86_64-linux = import ./pkgs {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        flake-inputs = inputs;
      };

      lib = import ./lib/pure.nix { inherit (nixpkgs) lib; };
      pkgs-lib.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.callPackage ./lib/pkgs.nix { };

      checks.x86_64-linux = import ./checks { flake-inputs = inputs; };

      devShells.x86_64-linux =
        (
          {
            self,
            nixpkgs,
            sops-nix,
            ...
          }:
          {
            default = nixpkgs.legacyPackages.mkShell {
              packages = nixpkgs.lib.attrValues {
                inherit (sops-nix.packages) sops-init-gpg-key sops-import-keys-hook;
              };

              sopsPGPKeyDirs = [
                "./keys/hosts/"
                "./keys/users/"
              ];
            };

            desktop-logic = self.packages.desktop-logic.devShell;
          }
        )
          (self.lib.flattenFlakeInputs inputs "x86_64-linux");
    };
}
