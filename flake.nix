{
  description = "tlater's dotfiles";

  inputs = {
    # NixOS related inputs
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
    impermanence.url = "github:nix-community/impermanence";

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-gaming = {
      url = "github:fufexan/nix-gaming";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-anywhere = {
      url = "github:numtide/nixos-anywhere";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        disko.follows = "disko";
      };
    };

    # home-manager related inputs
    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nurpkgs.url = "github:nix-community/NUR";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixd.url = "github:nix-community/nixd";

    deadnix.url = "github:astro/deadnix";
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      sops-nix,
      ...
    }@inputs:
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

        ren = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./nixos-config
            ./nixos-config/hosts/ren
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

      homeConfigurations = {
        # NixOS home configuration setup lives in
        # nixos-config/default.nix and their respective host-specific
        # modules.

        gnome-vm = home-manager.lib.homeManagerConfiguration {
          system = "x86_64-linux";
          username = "tlater";
          homeDirectory = "/home/tlater";

          configuration = ./home-config/hosts/gnome-vm.nix;
          extraSpecialArgs.flake-inputs = inputs;
        };
      };

      packages.x86_64-linux = import ./pkgs {
        inherit self;
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        flake-inputs = inputs;
      };

      apps.x86_64-linux.commit-nvfetcher = {
        type = "app";
        program = toString (
          nixpkgs.legacyPackages.x86_64-linux.writeShellScript "commit-nvfetcher" ''
            ${self.packages.x86_64-linux.commit-nvfetcher}/bin/commit-nvfetcher -k /tmp/github-key.toml
          ''
        );
      };

      checks.x86_64-linux = import ./checks {
        inherit (nixpkgs) lib;
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        flake-inputs = inputs;
      };

      devShells.x86_64-linux.default =
        let
          inherit (sops-nix.packages.x86_64-linux) sops-init-gpg-key sops-import-keys-hook;
          inherit (self.packages.x86_64-linux) commit-nvfetcher;
          inherit (nixpkgs.legacyPackages.x86_64-linux) nvchecker nvfetcher;
          home-manager-bin = home-manager.packages.x86_64-linux.default;
        in
        nixpkgs.legacyPackages.x86_64-linux.mkShell {
          packages = [
            nvfetcher
            nvchecker
            commit-nvfetcher
            home-manager-bin
            sops-init-gpg-key
          ];

          sopsPGPKeyDirs = [
            "./keys/hosts/"
            "./keys/users/"
          ];
          nativeBuildInputs = [ sops-import-keys-hook ];
        };
    };
}
