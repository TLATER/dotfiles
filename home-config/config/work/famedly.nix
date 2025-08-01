{
  pkgs,
  lib,
  flake-inputs,
  ...
}:
{
  programs = {
    git = {
      userEmail = "t.maat@famedly.com";

      signing = {
        key = "0x4D863FBF16FE6D51";
        signByDefault = true;
      };

      # For magit
      extraConfig.github.user = "famedly-tlater";
    };

    ssh.matchBlocks = {
      "*" = {
        identitiesOnly = true;
        identityFile = "~/.ssh/famedly-tlater.pub";
      };
    };

    firefox.enableThirdPartyRepositories = false;
  };

  home.packages = with pkgs; [
    bitwarden
    pre-commit
  ];

  home.file.".ssh/famedly-tlater.pub".source = "${flake-inputs.self}/keys/famedly-tlater.pub";

  xdg.configFile."autostart/work-sites.desktop".source =
    let
      desktopItem = pkgs.makeDesktopItem {
        name = "work-sites";
        desktopName = "Work site autostartup";
        exec = lib.concatStringsSep " " [
          "firefox"
          "https://calendar.google.com"
          "https://mail.google.com"
          "https://app.factorialhr.com/attendance/clock-in"
          "https://messenger.famedly.de"
        ];
      };
    in
    "${desktopItem}/share/applications/work-sites.desktop";
}
