/**
  Dev utilities.
*/
{ inputs, ... }: {
  imports = [ inputs.devshell.flakeModule ];

  perSystem =
    {
      config,
      pkgs,
      self',
      ...
    }:
    {
      devshells = {
        /**
          Default devshell containing all utilities for using this
          repository.
        */
        default = {
          name = "tlater’s dotfiles";

          packages = [
            config.treefmt.build.wrapper
            pkgs.nh
          ];

          commands = [
            {
              name = "update-packages";
              category = "[[maintenance commands]]";
              help = "update this flakes' packages";
              package = self'.builders.writeNuBinWith { packages = [ pkgs.git ]; } "update-packages" ./update.nu;
            }

            {
              name = "prek";
              category = "[[check commands]]";
              help = "Run and manage pre-commit hooks";
              package = config.prek.package.wrapper;
            }
          ];

          env = [
            {
              name = "NH_NO_CHECKS";
              value = true;
            }

            {
              name = "NH_FLAKE";
              value = "/home/tlater/.local/src/dotfiles";
            }
          ];
        };

        /**
          Rust devshell with the Rust toolchain required to develop on
          Rust projects in the repository.
        */
        rust = { extraModulesPath, ... }: {
          imports = [ "${extraModulesPath}/language/rust.nix" ];
          name = "tlater's dotfiles - rust edition";
        };
      };
    };
}
