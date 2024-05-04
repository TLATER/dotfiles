{ config, ... }:
{
  programs.firefox.webapps.element = {
    enable = true;

    url = "https://app.element.io";
    id = 2;
    extraSettings = config.programs.firefox.profiles."tlater".settings;
    # backgroundColor = "#";

    mimeType = [ "x-scheme-handler/element" ];
    categories = [
      "Network"
      "InstantMessaging"
      "Chat"
      "VideoConference"
    ];
  };
}
