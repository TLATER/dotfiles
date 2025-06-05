{
  config,
  pkgs,
  flake-inputs,
  ...
}:
let
  inherit (flake-inputs) self;
  inherit (flake-inputs.self.packages.${pkgs.system}) emacs;
in
{
  home.packages = with pkgs; [
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

    # *.nix files are used to pull in project deps, so we always need these
    nixfmt-rfc-style

    # Used for interactive python shells
    python3Packages.ipython

    # Required for markdown-mode (though could be replaced with a
    # different markdown implementation at some point)
    pandoc

    # To convert websites to readable text
    rdrview

    # Language servers and linters for super generic stuff
    biome # json/web stuff
    nil # nix
    ruff # python
    # *sh
    bash-language-server
    shellcheck
    shfmt
    yaml-language-server # yaml

    libnotify
    sqlite.dev
  ];

  xdg.configFile."emacs".source = "${self}/home-config/dotfiles/emacs.d/";

  programs.emacs = {
    enable = true;
    package = emacs;

    # The stylix config assumes idiomatic home-manager use, which does
    # not play nicely with emacs-overlay
    extraPackages =
      _:
      let
        epkgs = config.programs.emacs.package.emacs.pkgs;
        colors = config.lib.stylix.colors.withHashtag;
      in
      [
        (epkgs.melpaBuild {
          pname = "base16-stylix-theme";
          version = "0.1.0";
          src = pkgs.writeText "base16-stylix-theme.el" ''
            ; -*- lexical-binding: t; -*-
            (require 'base16-theme)

            (defvar base16-stylix-theme-colors
              '(:base00 "${colors.base00}"
                :base01 "${colors.base01}"
                :base02 "${colors.base02}"
                :base03 "${colors.base03}"
                :base04 "${colors.base04}"
                :base05 "${colors.base05}"
                :base06 "${colors.base06}"
                :base07 "${colors.base07}"
                :base08 "${colors.base08}"
                :base09 "${colors.base09}"
                :base0A "${colors.base0A}"
                :base0B "${colors.base0B}"
                :base0C "${colors.base0C}"
                :base0D "${colors.base0D}"
                :base0E "${colors.base0E}"
                :base0F "${colors.base0F}")
              "All colors for Base16 stylix are defined here.")

            ;; Define the theme
            (deftheme base16-stylix)

            ;; Add all the faces to the theme
            (base16-theme-define 'base16-stylix base16-stylix-theme-colors)

            ;; Mark the theme as provided
            (provide-theme 'base16-stylix)

            ;; Add path to theme to theme-path
            (add-to-list 'custom-theme-load-path
                (file-name-directory
                    (file-truename load-file-name)))

            (provide 'base16-stylix-theme)
          '';

          packageRequires = [ epkgs.base16-theme ];
        })
      ];
  };

  services.emacs = {
    enable = true;
    client = {
      enable = true;
      arguments = [
        "--no-wait"
        "--create-frame"
      ];
    };
    socketActivation.enable = true;
  };
}
