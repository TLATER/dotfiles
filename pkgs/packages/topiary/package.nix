{
  stdenv,
  flake-inputs,
  lib,
  callPackage,
  writeTextFile,
  symlinkJoin,
  makeBinaryWrapper,

  topiary,
  tree-sitter-grammars,

  localLib,
  nix-update,
}:
let
  inherit (flake-inputs.tree-sitter-sieve.packages.${stdenv.hostPlatform.system})
    tree-sitter-sieve
    topiary-sieve
    ;

  languages = {
    inherit topiary-sieve;
    topiary-nushell = callPackage ./topiary-nushell.nix { };
  };

  topiary-config = writeTextFile {
    name = "topiary-languages.ncl";
    destination = "/etc/topiary/languages.ncl";

    text = ''
      {
        languages = {
          nu = {
            extensions = ["nu"],
            grammar.source.path = "${tree-sitter-grammars.tree-sitter-nu}/parser"
          },

          sieve = {
            extensions = ["sieve"],
            grammar.source.path = "${tree-sitter-sieve}/parser"
          }
        }
      }
    '';
  };
in
symlinkJoin {
  inherit (topiary) pname version;

  paths = [
    topiary
    topiary-config
  ]
  ++ lib.attrValues languages;

  nativeBuildInputs = [ makeBinaryWrapper ];

  postBuild = ''
    wrapProgram $out/bin/topiary \
      --set TOPIARY_LANGUAGE_DIR "${placeholder "out"}/share/queries" \
      --set TOPIARY_CONFIG_FILE "${placeholder "out"}/etc/topiary/languages.ncl"
  '';

  passthru = {
    inherit languages;

    updateScript = localLib.writeUpdateScript {
      packageToUpdate = "topiary";
      utils = [ nix-update ];
      script = ''
        # Don't automatically update topiary-nushell for now, it's
        # very heavily tied to its tree-sitter grammar version.
        #
        # nix-update --flake --format --version=branch topiary.languages.topiary-nushell
      '';
    };
  };
}
