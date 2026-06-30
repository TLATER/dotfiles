{
  description = "tlater's dotfiles";

  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-26.05/nixexprs.tar.xz";
    flake-parts.url = "github:hercules-ci/flake-parts";

    wrappers = {
      url = "github:BirdeeHub/nix-wrapper-modules";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-gaming = {
      url = "github:fufexan/nix-gaming";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };

    nix-webapps.url = "github:TLATER/nix-webapps?ref=tlater/idiomatic-flake";

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # TODO: Remove when this hits the current home-manager release
    # branch (or remove it along with home-manager).
    home-manager-fix-ssh-socket = {
      url = "github:nix-community/home-manager?rev=55b927d6ebeeee9aadd70135125a35d80573ad6a";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs";
      };
    };

    tree-sitter-sieve = {
      url = "github:TLATER/tree-sitter-sieve";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-ast-lint = {
      url = "https://codeberg.org/tlater/nix-ast-lint/archive/main.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flint = {
      url = "github:NotAShelf/flint";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-flatpak.url = "github:gmodena/nix-flatpak/latest";
    famedly-nixos = {
      url = "github:famedly/famedly-nixos";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-compat.follows = "nix-gaming/git-hooks/flake-compat";
      };
    };
  };

  outputs =
    {
      devshell,
      flake-parts,
      nixpkgs,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./dev/devshells.nix
        ./dev/formatting.nix
        ./dev/pre-commit-hooks.nix
        ./lib/builders.nix
      ];

      systems = [ "x86_64-linux" ];

      flake = {
        nixosConfigurations = {
          rin = nixpkgs.lib.nixosSystem {
            modules = [
              ./nixos/hosts/rin.nix
              # TODO: Replace with hjem
              ./nixos/home-manager.nix
            ];

            specialArgs.inputs = inputs;
          };
        };

        lib = import ./lib/pure.nix { inherit (inputs.nixpkgs) lib; };
      };

      perSystem =
        {
          pkgs,
          inputs',
          self',
          ...
        }:
        {
          packages = pkgs.lib.packagesFromDirectoryRecursive {
            callPackage = pkgs.lib.callPackageWith (pkgs // { inherit inputs inputs' self'; });
            directory = ./packages;
          };
        };
    };
}
