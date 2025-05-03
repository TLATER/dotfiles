{ pkgs, flake-inputs, ... }:
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

    # Language servers and linters for super generic stuff
    biome # json/web stuff
    nixd # nix
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
