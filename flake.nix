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
    alejandra = {
      url = "github:kamadorueda/alejandra/1.1.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    nixpkgs,
    home-manager,
    nurpkgs,
    flake-utils,
    nvfetcher,
    alejandra,
    ...
  }: let
    overlays = [
      (final: prev: {
        local = import ./nixpkgs/pkgs {pkgs = prev;};
        alejandra = alejandra.defaultPackage.${prev.system};
      })
      nvfetcher.overlay
      nurpkgs.overlay
    ];
  in
    rec {
      lib = import ./nixpkgs/lib {inherit nixpkgs home-manager overlays;};

      # This defines home manager configurations that can either be
      # imported from the NixOS module, or used with home-manager's
      # homeManagerConfiguration function.
      profiles = import ./nixpkgs/profiles.nix;

      homeConfigurations = {
        # Sadly, this currently doesn't allow defining systems with
        # eachDefaultSystem, so we just assume everything is
        # x86_64-linux until it is not.
        graphical = lib.homeConfigurationFromProfile profiles.minimal.graphical {
          system = "x86_64-linux";
        };

        # The default configuration is just the minimal profile
        tlater = lib.homeConfigurationFromProfile profiles.minimal.text {
          system = "x86_64-linux";
        };
      };
    }
    // (flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit overlays system;};
      inherit (pkgs.lib.lists) optional optionals;
      inherit (pkgs.lib.strings) hasSuffix;
    in {
      packages = self.lib.localPackagesExcept system (
        # Work around https://github.com/NixOS/nix/issues/4265
        # TODO: Stop using IFD
        optional (system != "x86_64-linux") "emacs"
        ++ optionals (! hasSuffix "-linux" system) [
          # Packages with Linux-only dependencies
          "cap"
          "gcs"
          "pass-rofi"
          "setup-wacom"
        ]
      );

      devShell = pkgs.mkShell {
        buildInputs = with pkgs;
          [
            nixfmt
            nvfetcher-bin
            home-manager.defaultPackage.${system}
            local.commit-nvfetcher
          ]
          ++
          # For actual development - largely broken on non-x86_64-linux, so
          # we only install those there
          optional (system == "x86_64-linux") (python3.withPackages (ppkgs:
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
            ]));
      };
    }));
}
