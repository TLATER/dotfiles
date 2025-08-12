{
  imports = [
    ./keepassxc.nix
    ./ssh-servers.nix
    ./webapps.nix
  ];

  allowThirdPartyPackages = true;

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
