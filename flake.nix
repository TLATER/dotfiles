{
  description = "tlater's dotfiles";

  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-25.05/nixexprs.tar.xz";
    nixpkgs-unstable.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz";
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
      url = "github:nix-community/home-manager/release-25.05";
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

    nix-flatpak.url = "github:gmodena/nix-flatpak/latest";
    famedly-nixos = {
      url = "github:famedly/famedly-nixos";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, sops-nix, ... }@inputs:
    {
      nixosConfigurations = {
        yui = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./nixos-config
            ./nixos-config/hosts/yui
          ];

          specialArgs.flake-inputs = inputs;
        };

        rin = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./nixos-config
            ./nixos-config/hosts/rin
          ];

          specialArgs.flake-inputs = inputs;
        };
      };

      nixosModules = {
        nvidia = import ./nixos-modules/nvidia;
      };

      packages.x86_64-linux = import ./pkgs {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        flake-inputs = inputs;
      };

      lib = import ./lib/pure.nix { inherit (nixpkgs) lib; };
      pkgs-lib.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.callPackage ./lib/pkgs.nix { };

      checks.x86_64-linux = import ./checks { flake-inputs = inputs; };

      devShells.x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux.mkShell {
        packages = nixpkgs.lib.attrValues {
          inherit (sops-nix.packages.x86_64-linux) sops-init-gpg-key sops-import-keys-hook;
        };

        sopsPGPKeyDirs = [
          "./keys/hosts/"
          "./keys/users/"
        ];
      };
    };
}
