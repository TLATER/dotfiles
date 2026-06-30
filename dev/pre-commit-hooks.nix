{
  flake-parts-lib,
  inputs,
  lib,
  ...
}:
{
  options.perSystem = flake-parts-lib.mkPerSystemOption (
    { config, pkgs, ... }:
    let
      settingsFormat = pkgs.formats.toml { };
    in
    {
      options.prek = {
        package = lib.mkOption {
          type = inputs.wrappers.lib.types.subWrapperModule {
            imports = [ inputs.wrappers.lib.modules.default ];

            pkgs = lib.mkDefault pkgs;
            package = lib.mkDefault pkgs.prek;

            flags."--config" = settingsFormat.generate "prek.toml" config.prek.settings;
          };
        };

        settings = lib.mkOption { inherit (settingsFormat) type; };
      };
    }
  );

  config.perSystem = { config, pkgs, ... }: {
    prek = {
      package.runtimePkgs = [ config.treefmt.build.wrapper ];

      settings.repos = [
        {
          repo = "builtin";

          hooks = [
            { id = "check-added-large-files"; }
            { id = "check-case-conflict"; }
            { id = "check-executables-have-shebangs"; }
            { id = "check-json"; }
            { id = "check-json5"; }
            { id = "check-merge-conflict"; }
            { id = "check-shebang-scripts-are-executable"; }
            { id = "check-symlinks"; }
            { id = "check-toml"; }
            { id = "check-vcs-permalinks"; }
            { id = "check-xml"; }
            { id = "check-yaml"; }
            { id = "destroyed-symlinks"; }
            { id = "detect-private-key"; }
            { id = "end-of-file-fixer"; }
            {
              id = "mixed-line-ending";
              args = [ "--fix=lf" ];
            }
            { id = "trailing-whitespace"; }
          ];
        }

        {
          repo = "local";

          hooks = [
            {
              id = "treefmt";
              name = "treefmt";
              description = "Format *all* files";

              entry = "treefmt";
              language = "system";
            }
          ];
        }
      ];
    };
  };
}
