{
  description = "tlater's dotfiles";

  inputs = {
    # NixOS related inputs
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-unfree = {
      url = "github:numtide/nixpkgs-unfree";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    hyprland.url = "github:hyprwm/Hyprland";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
    peerix = {
      url = "github:cid-chan/peerix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # home-manager related inputs
    home-manager = {
      url = "github:nix-community/home-manager/release-22.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nurpkgs.url = "github:nix-community/NUR";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    sops-nix,
    ...
  } @ inputs: {
    nixosConfigurations = {
      yui = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./nixos-config
          ./nixos-config/yui
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
    };

    apps.x86_64-linux.commit-nvfetcher = {
      type = "app";
      program = toString (nixpkgs.legacyPackages.x86_64-linux.writeShellScript "commit-nvfetcher" ''
        ${self.packages.x86_64-linux.commit-nvfetcher}/bin/commit-nvfetcher -k /tmp/github-key.toml
      '');
    };

    checks.x86_64-linux = import ./checks {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      lib = nixpkgs.lib;
      flake-inputs = inputs;
    };

    devShells.x86_64-linux.default = let
      inherit (sops-nix.packages.x86_64-linux) sops-init-gpg-key sops-import-keys-hook;
      commit-nvfetcher = self.packages.x86_64-linux.commit-nvfetcher;
      home-manager-bin = home-manager.packages.x86_64-linux.default;
    in
      nixpkgs.legacyPackages.x86_64-linux.mkShell {
        packages = [
          commit-nvfetcher
          home-manager-bin
          sops-init-gpg-key
        ];

        sopsPGPKeyDirs = ["./keys/hosts/" "./keys/users/"];
        nativeBuildInputs = [
          sops-import-keys-hook
        ];
      };
  };
}
