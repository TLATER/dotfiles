{lib, flake-inputs, ...}: {
  programs.git = {
    userEmail = "t.maat@famedly.com";

    signing = {
      key = "0x4D863FBF16FE6D51";
      signByDefault = true;
    };

    # For magit
    extraConfig.github.user = "famedly-tlater";
  };

  home.file.".ssh/famedly-tlater.pub".source = "${flake-inputs.self}/keys/famedly-tlater.pub";

  programs.ssh.matchBlocks = {
    "*" = {
      identitiesOnly = true;
      identityFile = "~/.ssh/famedly-tlater.pub";
    };
  };

  programs.firefox.webapps = {
    discord.enable = lib.mkForce false;
    whatsapp.enable = lib.mkForce false;
    element.enable = lib.mkForce false;
  };
}
