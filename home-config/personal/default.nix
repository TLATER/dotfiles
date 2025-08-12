{ flake-inputs, pkgs, ... }:
{
  imports = [
    ./ssh-servers.nix
    ./webapps.nix
  ];

  allowThirdPartyPackages = true;

  home.packages = with pkgs; [
    prismlauncher
    flake-inputs.self.packages.${pkgs.system}.gcs
    flake-inputs.self.packages.${pkgs.system}.edopro
  ];

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
