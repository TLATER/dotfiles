{ pkgs, flake-inputs, ... }:
let
  inherit (flake-inputs) self;
  inherit (flake-inputs.self.packages.${pkgs.system}) emacs;
  inherit (flake-inputs.nixd.packages.${pkgs.system}) nixd;
in
{
  home.packages =
    (with pkgs; [
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

      # I just edit shell scripts often enough it makes sense to have
      # this by default
      shellcheck

      # Required for markdown-mode (though could be replaced with a
      # different markdown implementation at some point)
      pandoc

      # Technically only for web dev, but it does lsp stuff for JSON,
      # so...
      biome

      libnotify
      sqlite.dev
    ])
    ++ [ nixd ];

  xdg.configFile."emacs" = {
    source = "${self}/home-config/dotfiles/emacs.d/";
    recursive = true;
  };

  programs.emacs = {
    enable = true;
    package = emacs;
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
