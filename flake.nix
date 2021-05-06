{
  description = "tlater's home configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-20.09";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-20.09";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-unstable = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    nurpkgs = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, nixpkgs-unstable, home-manager, home-manager-unstable
    , nurpkgs, flake-utils, ... }:
    let
      overlays = [
        (final: prev: {
          unstable = import nixpkgs-unstable { system = prev.system; };
        })
        (final: prev: { local = import ./nixpkgs/pkgs { pkgs = prev; }; })
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
          # The homeManagerConfiguration function is only available on
          # unstable currently.
          home-manager-unstable.lib.homeManagerConfiguration {
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
        tlater = lib.homeConfigurationFromProfile profiles.minimal.graphical {
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
            home-manager-unstable.defaultPackage.${system}
          ];
        };
      }));
}
