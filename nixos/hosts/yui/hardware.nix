/**
  Additional hardware configuraiton for yui.

  TODO: Upstream this.
*/
{ lib, pkgs, ... }: {
  # Used for IPMI (remote maintenance thing), but is unsupported
  # by motherboard.
  boot.blacklistedKernelModules = [ "sp5100_tco" ];

  hardware.xpadneo.enable = true;

  # My JBL Cinema SB550 fails to connect with BlueZ 5.68
  hardware.bluetooth.package = lib.mkIf (lib.versionOlder pkgs.bluez.version "5.87") (
    (pkgs.bluez.override {
      bluez-headers = pkgs.bluez-headers.overrideAttrs (old: {
        version = "5.84";

        src = pkgs.fetchurl {
          url = "mirror://kernel/linux/bluetooth/bluez-5.84.tar.xz";
          hash = "sha256-W6c9Aw97AAh9Z4ALDjIWAa7A+JKCfHLlosg5DYyIaxE=";
        };
      });
    }).overrideAttrs
      (_: {
        patches = [
          (pkgs.fetchurl {
            name = "static.patch";
            url = "https://lore.kernel.org/linux-bluetooth/20250703182908.2370130-1-hi@alyssa.is/raw";
            hash = "sha256-4Yz3ljsn2emJf+uTcJO4hG/YXvjERtitce71TZx5Hak=";
          })
        ];
      })
  );

  services.udev.rules."98-b550i-suspend.rules" = ''
    # Fix broken suspend on b550i motherboard
    #
    # The rule is a bit overzealous, as it disables wake from *either*
    # NVME drive, but I don't see why anyone would want to wake from
    # NVME drives anyway.
    #
    # At least I *think* that's what the GPP bridge maps to. In
    # either case, this fixes the immediate resume from suspend on
    # my board.
    ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x1022", ATTR{device}=="0x1483", ATTR{power/wakeup}="disabled"
  '';
}
