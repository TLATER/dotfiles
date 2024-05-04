{ flake-inputs, pkgs, ... }:
{
  imports = [ ./ssh-servers.nix ];

  home.packages = with pkgs; [
    prismlauncher
    flake-inputs.self.packages.${pkgs.system}.gcs
  ];
  programs.firefox.enableThirdPartyRepositories = true;

  programs.git = {
    userEmail = "tm@tlater.net";

    signing = {
      key = "0x49670FD774E43268";
      signByDefault = true;
    };

    extraConfig = {
      # Magit-forge configuration
      github.user = "tlater";
      gitlab.user = "tlater";
    };
  };
}
