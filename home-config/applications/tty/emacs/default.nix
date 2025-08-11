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
  xdg.configFile."emacs".source = "${self}/home-config/dotfiles/emacs.d/";

  programs.emacs = {
    enable = true;
    package = emacs;

    extraPackages = _: [
      (emacs.emacs.emacs.pkgs.callPackage ./color-template.nix {
        colors = config.lib.stylix.colors.withHashtag;
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
