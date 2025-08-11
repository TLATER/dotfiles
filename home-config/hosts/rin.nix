{
  imports = [
    ../.
    ../applications
    ../desktop
    ../services
    ../shell
    ../xdg-settings.nix

    ../work/famedly.nix
  ];

  # Make sure the primary display is set before eww/wpaperd launch
  systemd.user.services.way-displays.Unit.Before = [
    "eww.service"
    "wpaperd.service"
  ];

  services.wayDisplays = {
    enable = true;
    settings =
      let
        builtin = "eDP-1";
        home-1 = "Dell Inc. DELL G2723HN 5B0C3H3";
        work-1 = "DELL U3219Q";
      in
      {
        ARRANGE = "COLUMN";

        ORDER = [
          work-1
          home-1
          builtin
        ];

        SCALING = true;

        SCALE = [
          {
            NAME_DESC = "eDP-1";
            SCALE = 1.0;
          }
        ];

        MODE = [
          {
            NAME_DESC = home-1;
            WIDTH = 1920;
            HEIGHT = 1080;
            Hz = 164.997;
          }
        ];

        VRR_OFF = [
          work-1 # Doesn't support VRR, this suppresses warnings

          # Currently VRR is borked with an amdgpu
          builtin
          home-1
        ];
      };
  };
}
