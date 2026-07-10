{
  lib,
  callPackage,
  writeTextFile,
  symlinkJoin,
  makeBinaryWrapper,

  topiary,

  self',
  inputs',
  nix-update,
  ast-grep,
  nix-prefetch-github,
  nixfmt,
}:
let
  inherit (inputs'.tree-sitter-sieve.packages) tree-sitter-sieve topiary-sieve;

  grammars = {
    inherit tree-sitter-sieve;
    tree-sitter-nu = callPackage ./tree-sitter-nu.nix { };
  };

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
            grammar.source.path = "${grammars.tree-sitter-nu}/parser"
          },

          sieve = {
            extensions = ["sieve"],
            grammar.source.path = "${grammars.tree-sitter-sieve}/parser"
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
    inherit grammars languages;

    updateScript = self'.builders.writeUpdateScript {
      packageToUpdate = "topiary";
      utils = [
        ast-grep
        nix-update
        nix-prefetch-github
        nixfmt
      ];
      script = ./update.nu;
    };
  };
}
