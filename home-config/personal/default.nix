{
  imports = [
    ./keepassxc.nix
    ./ssh-servers.nix
    ./webapps.nix
  ];

  programs.git = {
    signing = {
      key = "0x02E935006CF2E8E7";
      signByDefault = true;
    };

    settings = {
      user.email = "tm@tlater.net";
      # Magit-forge configuration
      github.user = "tlater";
      gitlab.user = "tlater";
    };
  };
}
