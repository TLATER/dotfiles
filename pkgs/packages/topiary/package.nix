{
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
  languages = {
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
    languages.topiary-nushell
  ];

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
