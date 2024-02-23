{
  programs.git = {
    userEmail = "t.maat@famedly.com";

    signing = {
      key = "";
      signByDefault = true;
    };

    # For magit
    extraConfig.github.user = "famedly-tlater";
  };

  programs.ssh.matchBlocks = {
    "*" = {
      identitiesOnly = true;
      identityFile = "~/.ssh/famedly-tlater.pub";
    };
  };

  programs.firefox.webapps = {
    discord.enable = false;
    whatsapp.enable = false;
    element.enable = false;
  };
}
