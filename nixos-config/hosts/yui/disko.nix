{ flake-inputs, ... }:
let
  inherit (flake-inputs.self.lib) mapSubvolumes;
in
{
  disko.devices.disk = {
    nvme0n1 = {
      type = "disk";
      device = "/dev/disk/by-id/nvme-eui.002538ba015048d0";
      content = {
        type = "gpt";
        partitions = {
          ESP = {
            label = "EFI";
            name = "ESP";
            size = "512M";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = [
                "defaults"
                "umask=0077"
              ];
            };
          };

          main = {
            end = "-32G";
            content = {
              type = "luks";
              name = "main";
              passwordFile = "/tmp/secret.key";
              settings.allowDiscards = true;
              content = {
                type = "btrfs";
                extraArgs = [ "-f" ];
                subvolumes = mapSubvolumes {
                  "/root" = "/";
                  "/home" = "/home";
                  "/var" = "/var";
                  "/nix" = "/nix";
                  "/snapshots" = {
                    mountpoint = "/snapshots/main";
                  };
                };
              };
            };
          };

          swap = {
            size = "100%";
            content = {
              type = "swap";
              randomEncryption = true;
              resumeDevice = true;
            };
          };
        };
      };
    };

    nvme1n1 = {
      type = "disk";
      device = "/dev/disk/by-id/nvme-Sabrent_Rocket_Q_BD520704027D01592244";
      content = {
        type = "luks";
        name = "storage";
        passwordFile = "/tmp/secret.key";
        settings.allowDiscards = true;
        content = {
          type = "btrfs";
          extraArgs = [ "-f" ];
          subvolumes = mapSubvolumes {
            "/root" = "/storage";
            "/steam" = "/storage/steam";
            "/media" = "/storage/media";
            "/snapshots" = {
              mountpoint = "/snapshots/storage";
            };
          };
        };
      };
    };
  };
}
