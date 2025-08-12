{ pkgs, flake-inputs, ... }:
{
  allowThirdPartyPackages = false;

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
  };

  home.packages = with pkgs; [
    bitwarden
    pre-commit
  ];

  home.file.".ssh/famedly-tlater.pub".source = "${flake-inputs.self}/keys/famedly-tlater.pub";
}
