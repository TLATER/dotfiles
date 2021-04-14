{
  description = "tlater's home configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-20.09";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-20.09";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nurpkgs = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, nixpkgs-unstable, home-manager, nurpkgs, flake-utils, ... }:
    let
      overlays = [
        (final: prev: {
          unstable = import nixpkgs-unstable { system = prev.system; };
        })
        (final: prev: { local = import ./nixpkgs/pkgs { pkgs = prev; }; })
        nurpkgs.overlay
      ];

      # A homeManagerConfiguration that can be pulled into the
      # `home-manager.users.${user}` option - distinct from the
      # upstream library function, since that one defines modules that
      # will conflict with the ones imported by the system.
      nixOSHomeManagerConfiguration = { system, username ? "tlater"
        , homeDirectory ? "/home/${username}", modules ? [ ] }: {
          nixpkgs.overlays = overlays;

          imports = [ ./nixpkgs ] ++ modules;
        };

    in {
      homeConfigurations = {
        yui = nixOSHomeManagerConfiguration rec {
          system = "x86_64-linux";
          modules = [
            ./nixpkgs/configurations/graphical-programs
            ./nixpkgs/configurations/tty-programs
            ./nixpkgs/configurations/graphical-programs/games.nix

            ./nixpkgs/configurations/graphical-programs/barrier-server.nix
          ];
        };

        ct-lt-02052 = nixOSHomeManagerConfiguration {
          system = "x86_64-linux";
          modules = [
            ./nixpkgs/configurations/graphical-programs
            ./nixpkgs/configurations/tty-programs
            ./nixpkgs/configurations/tty-programs/work.nix

            ./nixpkgs/configurations/graphical-programs/barrier-client.nix
          ];
        };
      };
    }

    # Set up a "dev shell" that will work on all architectures
    // (flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit overlays system; };
      in {
        packages = import ./nixpkgs/pkgs { inherit pkgs; };
        devShell = pkgs.mkShell { buildInputs = with pkgs; [ nixfmt ]; };
      }));
}
