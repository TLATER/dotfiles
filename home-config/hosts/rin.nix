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

  services.wayDisplays = {
    enable = true;
    settings =
      let
        builtin = "eDP-1";
        home-1 = "!^Dell Inc. DELL G2723HN";
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
            HZ = 60;
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
