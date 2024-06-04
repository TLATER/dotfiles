{
  self,
  pkgs,
  flake-inputs,
}:
let
  sources = pkgs.callPackage ./sources.nix { };
  callPackage = pkgs.lib.callPackageWith (pkgs // { inherit self sources flake-inputs; });
in
{
  # "Packages" that really just contain configuration settings
  firefox-ui-fix = callPackage ./configuration/firefox-ui-fix.nix { };
  oh-my-zsh-bgnotify = callPackage ./configuration/oh-my-zsh-bgnotify.nix { };
  oh-my-zsh-emacs = callPackage ./configuration/oh-my-zsh-emacs.nix { };
  oh-my-zsh-screen = callPackage ./configuration/oh-my-zsh-screen.nix { };
  phosphor-icons = callPackage ./configuration/phosphor-icons.nix { };
  tridactyl-emacs = callPackage ./configuration/tridactyl-emacs.nix { };

  # "Packages" that just contain utility scripts
  commit-nvfetcher = callPackage ./scripts/commit-nvfetcher { };

  # Proper packages
  deepfilternet = callPackage ./applications/deepfilternet.nix { };
  drivestrike = callPackage ./applications/drivestrike.nix { };
  emacs = callPackage ./applications/emacs { };
  gauth = callPackage ./applications/gauth.nix { };
  gcs = callPackage ./applications/gcs.nix { };
  stumpwm = callPackage ./applications/stumpwm { };
  stumpwm-contrib = callPackage ./applications/stumpwm/stumpwm-contrib.nix { };
  nextcloudcmd = callPackage ./applications/nextcloudcmd.nix { };
}
