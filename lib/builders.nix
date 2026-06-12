/**
  Custom utility builders.
*/
{
  flake-parts-lib,
  inputs,
  lib,
  ...
}:
{
  imports = [
    (flake-parts-lib.mkTransposedPerSystemModule {
      name = "builders";
      option = lib.mkOption {
        type = lib.types.lazyAttrsOf lib.types.raw;
        default = { };

        description = ''
          Custom utility builders.
        '';
      };

      file = ./builders.nix;
    })
  ];

  perSystem =
    { self', pkgs, ... }:
    let
      inherit (pkgs) writers;
    in
    {
      builders = {

        /**
          An alternative to `pkgs.writers.writeNu` that allows adding
          packages and plugins more conveniently.

          # Type

          ```
          writeNuWith :: { packages :: [Derivation]; plugins :: [Derivation]; extraMakeWrapperArgs :: [String]; } -> String -> String | Path -> Derivation
          ```

          # Inputs

          `packages`
          : Packages to add to `$PATH` of the resulting script.

          `plugins`
          : Nushell plugins to add to the interpreter.

          `extraMakeWrapperArgs`
          : Additional arguments to pass to `makeWrapper`.
        */
        writeNuWith =
          {
            packages ? [ ],
            plugins ? [ ],
            extraMakeWrapperArgs ? [ ],
          }:
          writers.makeScriptWriter {
            interpreter =
              (inputs.wrappers.lib.evalModule {
                inherit pkgs;
                imports = [ inputs.wrappers.lib.modules.default ];

                package = pkgs.nushell;

                flags = {
                  "--plugins" = "[" + (lib.concatStringsSep " " (map lib.getExe plugins)) + "]";
                  "--no-config-file" = true;
                };
              }).config.wrapper;

            makeWrapperArgs =
              (lib.optionals (packages != [ ]) [
                "--prefix"
                "PATH"
                ":"
                (lib.makeBinPath packages)
              ])
              ++ extraMakeWrapperArgs;
          };

        /**
          An alternative to `pkgs.writers.writeNuBinWith` that allows
          adding packages and plugins more conveniently.

          # Type

          ```
          writeNuBinWith :: { packages :: [Derivation]; plugins :: [Derivation]; extraMakeWrapperArgs :: [String]; } -> String -> String | Path -> Derivation
          ```

          # Inputs

          `packages`
          : Packages to add to `$PATH` of the resulting script.

          `plugins`
          : Nushell plugins to add to the interpreter.

          `extraMakeWrapperArgs`
          : Additional arguments to pass to `makeWrapper`.
        */
        writeNuBinWith = args: name: self'.builders.writeNuWith args "/bin/${name}";

        /**
          Helper to create a script that updates a derivation using `nix-update`.

          # Type

          ```
          nixUpdateScript :: { packageToUpdate :: String; version :: String } -> Derivation
          ```

          # Inputs

          `packageToUpdate`
          : The name of the package to update.

          `version`
          : The version to update to.
        */
        nixUpdateScript =
          {
            packageToUpdate,
            version ? null,
          }:
          self'.builders.writeNuBinWith
            {
              packages = [
                pkgs.nix-update
                pkgs.nixfmt

              ];
            }
            "update-${packageToUpdate}"
            ''
              (nix-update
                --flake
                --format
                ${lib.concatStringsSep " " (lib.optional (version != null) "--version=${version}")}
                ${packageToUpdate})
            '';

        /**
          Helper to create a nushell script that updates a package.

          # Type

          ```
          writeUpdateScript :: { script :: String | Path; packageToUpdate :: String; utils :: [Derivation]; nushellPlugins :: [Derivation] } -> Derivation
          ```

          # Inputs

          `script`
          : The script text.

          `packageToUpdate`
          : The name of the package to update.

          `utils`
          : Utility packages to make available to the script.

          `nushellPlugins`
          : Plugins to add to nushell.
        */
        writeUpdateScript =
          {
            script,
            packageToUpdate,
            utils ? [ ],
            nushellPlugins ? [ ],
          }:
          self'.builders.writeNuBinWith {
            packages = utils;
            plugins = nushellPlugins;
          } "update-${packageToUpdate}" script;
      };
    };
}
