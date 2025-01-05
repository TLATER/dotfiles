{
  description = "tlater's dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    impermanence.url = "github:nix-community/impermanence";

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
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
        nixos-stable.follows = "nixpkgs";
      };
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nurpkgs = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
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

      nixosModules = {
        nvidia = import ./nixos-modules/nvidia;
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
          inherit (nixpkgs.legacyPackages.x86_64-linux)
            cargo
            clippy
            dbus
            nvchecker
            nvfetcher
            pkg-config
            rust-analyzer
            rustc
            rustfmt
            ;
          home-manager-bin = home-manager.packages.x86_64-linux.default;
        in
        nixpkgs.legacyPackages.x86_64-linux.mkShell {
          packages = [
            nvfetcher
            nvchecker
            commit-nvfetcher
            home-manager-bin
            sops-init-gpg-key

            cargo
            clippy
            dbus
            rust-analyzer
            rustc
            rustfmt
            pkg-config
          ];

          sopsPGPKeyDirs = [
            "./keys/hosts/"
            "./keys/users/"
          ];
          nativeBuildInputs = [ sops-import-keys-hook ];
        };
    };
}
