{
  imports = [
    ./keepassxc.nix
    ./ssh-servers.nix
    ./webapps.nix
  ];

  programs.git = {
    userEmail = "tm@tlater.net";

    signing = {
      key = "0x02E935006CF2E8E7";
      signByDefault = true;
    };

    extraConfig = {
      # Magit-forge configuration
      github.user = "tlater";
      gitlab.user = "tlater";
    };
  };
}
