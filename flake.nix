{
  description = "tlater's home configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nurpkgs = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    nvfetcher = {
      url = "github:berberman/nvfetcher";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { nixpkgs, home-manager, nurpkgs, flake-utils, nvfetcher, ... }:
    let
      overlays = [
        (final: prev: { local = import ./nixpkgs/pkgs { pkgs = prev; }; })
        nvfetcher.overlay
        nurpkgs.overlay
      ];

    in rec {
      lib = rec {
        # Create a module with correctly set overlays from a given
        # profile.
        #
        nixosModuleFromProfile = profile:
          { ... }@args:
          (profile args) // {
            nixpkgs.overlays = overlays;
          };

        # Create a NixOS module that configures home-manager to use
        # the given profile.
        #
        nixosConfigurationFromProfile = profile: username:
          { ... }@args: {
            home-manager.users.${username} = nixosModuleFromProfile profile;
          };

        # Create a homeManagerConfiguration that can be installed
        # using `home-manager --flake`.
        #
        homeConfigurationFromProfile = profile:
          { system, username ? "tlater", homeDirectory ? "/home/${username}" }:
          home-manager.lib.homeManagerConfiguration {
            inherit homeDirectory system username;
            configuration = nixosModuleFromProfile profile;
          };
      };

      # This defines home manager configurations that can either be
      # imported from the NixOS module, or used with home-manager's
      # homeManagerConfiguration function.
      profiles = import ./nixpkgs/profiles.nix;

      homeConfigurations = {
        # Sadly, this currently doesn't allow defining systems with
        # eachDefaultSystem, so we just assume everything is
        # x86_64-linux until it is not.
        graphical =
          lib.homeConfigurationFromProfile profiles.minimal.graphical {
            system = "x86_64-linux";
          };

        # The default configuration is just the minimal profile
        tlater = lib.homeConfigurationFromProfile profiles.minimal.text {
          system = "x86_64-linux";
        };
      };
    }

    # Set up a "dev shell" that will work on all architectures
    // (flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit overlays system; };
      in {
        packages = import ./nixpkgs/pkgs { inherit pkgs; };
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            nixfmt
            nvfetcher-bin
            home-manager.defaultPackage.${system}
            local.commit-nvfetcher

            # For python scripts
            (python3.withPackages (ppkgs:
              with ppkgs; [
                python-lsp-server
                python-lsp-black
                pyls-isort
                pylsp-mypy

                rope
                pyflakes
                mccabe
                pycodestyle
                pydocstyle
              ]))
          ];
        };
      }));
}
