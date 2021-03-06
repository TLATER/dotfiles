{
  description = "tlater's home configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-20.09";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/release-20.09";
    nurpkgs.url = "github:nix-community/NUR";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    let
      make-home = { username ? "tlater", homeDirectory ? "/home/${username}"
        , modules ? [ ] }: {
          nixpkgs.overlays = [
            (final: prev: {
              unstable =
                import inputs.nixpkgs-unstable { system = prev.system; };
            })
            (final: prev: {
              local = import ./nixpkgs/pkgs {
                pkgs = prev;
                unstable-pkgs = prev.unstable;
              };
            })
            inputs.nurpkgs.overlay
          ];

          imports = [ ./nixpkgs ] ++ modules;
        };

    in {
      homeConfigurations = {
        yui = make-home {
          modules = [
            ./nixpkgs/configurations
            ./nixpkgs/configurations/graphical-programs
            ./nixpkgs/configurations/tty-programs
            ./nixpkgs/configurations/graphical-programs/games.nix
          ];
        };
        ct-lt-02052 = make-home {
          modules = [
            ./nixpkgs/configurations
            ./nixpkgs/configurations/graphical-programs
            ./nixpkgs/configurations/tty-programs
            ./nixpkgs/configurations/tty-programs/work.nix
          ];
        };
      };
    }
    # Set up a "dev shell" that will work on all architectures
    // (inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        unstable-pkgs = inputs.nixpkgs-unstable.legacyPackages.${system};
      in {
        packages = import ./nixpkgs/pkgs { inherit pkgs unstable-pkgs; };
        devShell = pkgs.mkShell { buildInputs = with pkgs; [ nixfmt ]; };
      }));
}
