{
  pkgs,
  lib,
  flake-inputs,
  ...
}:
{
  programs = {
    aria2.enable = lib.mkForce false;
    ncmpcpp.enable = lib.mkForce false;

    librewolf = {
      enableLepton = lib.mkForce false;

      extensions.installByPolicy = [
        "bitwarden-password-manager"
        "canvasblocker"
        "ublock-origin"
      ];

      # Required for the Famedly webapp, so I can't realistically work
      # without this
      settings."webgl.disabled" = lib.mkForce false;
    };

    git = {
      signing = {
        key = "0x4D863FBF16FE6D51";
        signByDefault = true;
      };

      # For magit
      settings = {
        user.email = "t.maat@famedly.com";
        github.user = "famedly-tlater";
      };
    };

    ssh = {
      enableDefaultConfig = false;
      matchBlocks."*".identityFile = "~/.ssh/famedly-tlater.pub";
    };
  };

  services.mpd.enable = lib.mkForce false;

  home.packages = with pkgs; [
    bitwarden-desktop
    pre-commit
  ];

  home.file.".ssh/famedly-tlater.pub".source = "${flake-inputs.self}/keys/famedly-tlater.pub";
}
