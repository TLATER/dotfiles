{
  config,
  lib,
  ...
}: {
  config = lib.mkIf config.custom.desktop-environment {
    programs.ncmpcpp = {
      enable = true;
      settings = {
        ncmpcpp_directory = "${config.xdg.dataHome}/ncmpcpp";
        lyrics_directory = "${config.xdg.dataHome}/lyrics";
      };
    };

    services.mpd = {
      enable = true;
      musicDirectory = config.xdg.userDirs.music;
      network.startWhenNeeded = true;
    };
  };
}
