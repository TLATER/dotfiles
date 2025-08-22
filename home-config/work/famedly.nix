{ pkgs, flake-inputs, ... }:
{
  allowThirdPartyPackages = false;

  programs = {
    aria2.enable = lib.mkForce false;
    ncmpcpp.enable = lib.mkForce false;

    librewolf = {
      # Required for the Famedly webapp, so I can't realistically work
      # without this
      settings."webgl.disabled" = lib.mkForce false;
    };

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
  };

  services.mpd.enable = lib.mkForce false;

  home.packages = with pkgs; [
    bitwarden
    pre-commit
  ];

  home.file.".ssh/famedly-tlater.pub".source = "${flake-inputs.self}/keys/famedly-tlater.pub";
}
