{
  pkgs,
  flake-inputs,
  sources,
  lib,
  hostPlatform,
  emacsMacport,
  emacs30-pgtk,
  symlinkJoin,
}:
let
  package = if hostPlatform.isDarwin then emacsMacport else emacs30-pgtk;
  emacsDotfiles = "${flake-inputs.self}/home-config/dotfiles/emacs.d";

  emacsWithPackagesFromUsePackage = import "${flake-inputs.emacs-overlay}/elisp.nix" {
    inherit pkgs;
  };

  # Compute the list of leaf-d packages.
  emacsConfigs =
    [ "${emacsDotfiles}/init.el" ]
    ++ (lib.mapAttrsToList (fname: _: "${emacsDotfiles}/config/${fname}") (
      lib.filterAttrs (_: type: type == "regular") (builtins.readDir "${emacsDotfiles}/config/")
    ));

  runtimeDeps = with pkgs; [
    # Spell checks
    (aspellWithDicts (
      dicts: with dicts; [
        af
        de
        en
        en-computers
        nl
      ]
    ))

    # Used for interactive python shells
    python3Packages.ipython

    # Required for markdown-mode (though could be replaced with a
    # different markdown implementation at some point)
    pandoc

    # To convert websites to readable text
    rdrview

    # Language servers, linters and formatters
    biome # json/web stuff
    nil # nix
    nixfmt-rfc-style
    ruff # python
    # *sh
    bash-language-server
    shellcheck
    shfmt
    yaml-language-server # yaml

    # Desktop notifications
    libnotify
  ];

  emacs = emacsWithPackagesFromUsePackage {
    inherit package;

    # TODO(tlater): Consider adding multi-file support upstream.
    config = lib.concatMapStringsSep "\n" builtins.readFile emacsConfigs;
    extraEmacsPackages = epkgs: [
      (epkgs.treesit-grammars.with-grammars (
        grammars: with grammars; [
          tree-sitter-bash
          tree-sitter-css
          tree-sitter-dockerfile
          tree-sitter-json
          tree-sitter-nix
          tree-sitter-nu
          tree-sitter-python
          tree-sitter-rust
          tree-sitter-typescript
          tree-sitter-tsx
          tree-sitter-yaml
        ]
      ))
    ];
    override = self: _super: {
      eglot-x = self.callPackage ./eglot-x.nix { inherit sources; };
      kdl-mode = self.callPackage ./kdl-mode.nix { inherit sources; };
    };
  };
in
symlinkJoin {
  name = "emacs-with-runtimedeps";
  paths = [ emacs ];
  nativeBuildInputs = [ pkgs.makeWrapper ];

  passthru.emacs = emacs;

  # TODO(tlater): Seems the upstream package already creates binary
  # wrappers for the various emacs `bin/` binaries, since they are all
  # scripts and this prevents using emacs as a shebang on darwin.
  #
  # The upstream wrapper is a noop, however, so it'd be way cleaner to
  # just replace the upstream wrapper with our own, to avoid
  # double-wrapping.
  #
  # See:
  # https://github.com/NixOS/nixpkgs/blob/611de7d38b1da74e4edc2db1e9576bee6e3d6eea/pkgs/applications/editors/emacs/build-support/wrapper.nix#L216
  postBuild = ''
    rm $out/bin/emacs
    makeWrapper ${emacs}/bin/emacs $out/bin/emacs --prefix PATH : ${lib.makeBinPath runtimeDeps}
  '';
}
