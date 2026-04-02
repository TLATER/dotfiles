{
  flake-inputs,
  config,
  pkgs,
  ...
}:
let
  inherit (flake-inputs.self.pkgs-lib.${pkgs.stdenv.hostPlatform.system}) writeNuWith;
  inherit (flake-inputs.self.packages.${pkgs.stdenv.hostPlatform.system}) edid-generator;
in
{
  nix.settings = {
    substituters = [ "https://cache.nixos-cuda.org" ];
    trusted-public-keys = [ "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M=" ];
  };

  # Create a virtual display to render games for sunshine on
  hardware.display = {
    edid.packages = [
      (pkgs.runCommand "VTCL65C825.bin"
        {
          nativeBuildInputs = [ edid-generator ];
          settings = pkgs.writers.writeJSON "edid.json" {
            defaultMode = {
              width = 1920;
              height = 1080;
              refresh = 120;
            };
            modes = [
              {
                width = 1920;
                height = 1080;
                refresh = 120;
              }
              {
                width = 1920;
                height = 1080;
                refresh = 60;
              }
              {
                width = 3840;
                height = 2160;
                refresh = 120;
              }
              {
                width = 3840;
                height = 2160;
                refresh = 60;
              }
            ];
            audio = false;
            hdr = true;
            deepColor = true;
            dsc = false;
            vrr = false;
            listedModesOnly = false;
          };
        }
        ''
          mkdir -p "$out/lib/firmware/edid"
          edid-generator $settings > "$out/lib/firmware/edid/VTCL65C825.bin"
        ''
      )
    ];

    # The port (DP-2) to be used must be free on the GPU, use
    # `ls /sys/class/drm/*/status` to find free ports.
    #
    # Using DP-2 because HDMI doesn't support 3840x2160@120.
    outputs."DP-2" = {
      edid = "VTCL65C825.bin";
      mode = "e";
    };
  };

  services.sunshine = {
    enable = true;
    package = pkgs.sunshine.override { cudaSupport = true; };

    openFirewall = true;
    settings = {
      sunshine_name = config.networking.hostName;
      system_tray = false;
      encoder = "nvenc";

      # TODO(tlater): Switch to portalgrab once released:
      # https://github.com/LizardByte/Sunshine/pull/4417
      capture = "wlr";
      output_name = 1;
    };

    applications.apps = [
      (
        let
          runScript = writeNuWith {
            packages = [
              pkgs.util-linux
              config.programs.sway.package
              config.programs.steam.package
            ];
          };
        in
        {
          name = "Steam Big Picture";
          cmd =
            (runScript "run-steam" /* nu */ ''
              swaymsg output DP-2 enable
              swaymsg workspace sunshine gaps outer 0
              swaymsg workspace sunshine gaps inner 0
              swaymsg workspace sunshine output DP-2
              swaymsg focus output DP-2
              swaymsg assign '[title="Steam Big Picture Mode"]' → workspace steam

              try {
                setsid steam steam://open/bigpicture
              }
            '').outPath;

          auto-detach = false;
          prep-cmd = [
            {
              undo =
                (runScript "close-steam" /* nu */ ''
                  swaymsg output DP-2 disable

                  sleep 5sec
                  if (ps | where pid == (cat ~/.steampid)) != [ ] {
                    try {
                      steam -shutdown
                    }
                  }
                '').outPath;
            }
          ];
        }
      )
    ];
  };

}
