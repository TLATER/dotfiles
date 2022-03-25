{pkgs, ...}: let
  screenHeight = 1080;
  screenWidth = 1920;
in {
  services = {
    dunst = {
      enable = true;
      settings = {
        global = {
          follow = "keyboard";
          indicate_hidden = "yes";

          width = "${toString (screenWidth / 2)}";
          height = "100";
          origin = "bottom-center";
          offset = "5x28";
          notification_limit = "2";
          separator_height = "2";
          padding = "8";
          horizontal_padding = "8";
          frame_width = "1";
          alignment = "center";
          line_height = "0";
          font = "Monospace 12";
          markup = "full";
          format = "<b>%s</b>\\n%b";

          min_icon_size = "${toString (screenHeight / 10)}";
          max_icon_size = "${toString (screenHeight / 10)}";

          frame_color = "#0a3749";
          separator_color = "frame";
        };

        urgency_low = {
          background = "#222222";
          foreground = "#888888";
        };

        urgency_normal = {
          background = "#0f0f0f";
          foreground = "#99d1ce";
        };

        urgency_critical = {
          background = "#900000";
          foreground = "#ffffff";
          frame_color = "#ff0000";
          timeout = 0;
        };

        shortcuts = {
          close = "ctrl+grave";
          close_all = "ctrl+shift+grave";
          history = "ctrl+shift+#";
          context = "ctrl+shift+period";
        };
      };
    };
  };
}
