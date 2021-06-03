{ pkgs, ... }:

let
  screenHeight = 1080;
  screenWidth = 1920;

  # Make an explicitly signed string from an int (e.g., "+2", "-1")
  intToSignedString = int:
    if int < 0 then (toString int) else "+${toString int}";

  # Create a geometry string as dunst uses them.
  #
  # Args:
  #    width: The width of the notification in pixels
  #    maxShown: The max number of notifications to show at a time
  #    marginLR: The margin from the sides. Positive numbers create a
  #              left margin, negative numbers a right one.
  #    marginTB: The margin from the top/bottom. Positive numbers
  #              create a top margin, negative numbers a bottom one.
  #
  geometryString = width: maxShown: marginLR: marginTB:
    "${toString width}x${toString maxShown}${intToSignedString marginLR}${
      intToSignedString marginTB
    }";

in {
  services = {
    dunst = {
      enable = true;
      settings = {
        global = {
          follow = "keyboard";
          indicate_hidden = "yes";

          geometry = geometryString (screenWidth / 2) 5 (screenWidth / 4) (-28);
          notification_height = "0";
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
