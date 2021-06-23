{ pkgs, config, ... }:

# Configuration for "personal computers", i.e., physical computers in
# front of me.
#
# These need things like access to encryption keys, passwords and
# music players.

{
  programs = {
    git.signing.signByDefault = true;

    ncmpcpp = {
      enable = true;
      settings = {
        ncmpcpp_directory = "${config.xdg.dataHome}/ncmpcpp";
        lyrics_directory = "${config.xdg.dataHome}/lyrics";
      };
    };

    password-store = {
      enable = true;
      settings = {
        PASSWORD_STORE_DIR = "${config.xdg.dataHome}/password-store";
        PASSWORD_STORE_KEY = "0x9FAF1AA48509A7F1";
        PASSWORD_STORE_GENERATED_LENGTH = "16";
      };
    };

    # The yubikey can only be present on PCs, so ssh on non-PCs will
    # need to work some other way.
    ssh.matchBlocks."*" = {
      identitiesOnly = true;
      identityFile = "~/.ssh/tlater.pub";
    };
  };

  services.mpd = {
    enable = true;
    musicDirectory = config.xdg.userDirs.music;
    network.startWhenNeeded = true;
  };
}
