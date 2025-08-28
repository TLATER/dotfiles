{
  disko.devices.disk = {
    nvme0n1 = {
      type = "disk";
      device = "/dev/nvme0n1";
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
                subvolumes = {
                  "/root" = {
                    mountpoint = "/";
                    mountOptions = [
                      "compress=zstd"
                      "noatime"
                    ];
                  };
                  "/home" = {
                    mountpoint = "/home";
                    mountOptions = [
                      "compress=zstd"
                      "noatime"
                    ];
                  };
                  "/var" = {
                    mountpoint = "/var";
                    mountOptions = [
                      "compress=zstd"
                      "noatime"
                    ];
                  };
                  "/nix" = {
                    mountpoint = "/nix";
                    mountOptions = [
                      "compress=zstd"
                      "noatime"
                    ];
                  };

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
      device = "/dev/nvme1n1";
      content = {
        type = "luks";
        name = "storage";
        passwordFile = "/tmp/secret.key";
        settings.allowDiscards = true;
        content = {
          type = "btrfs";
          extraArgs = [ "-f" ];
          subvolumes = {
            "/root" = {
              mountpoint = "/storage";
              mountOptions = [
                "compress=zstd"
                "noatime"
              ];
            };
            "/steam" = {
              mountpoint = "/storage/steam";
              mountOptions = [
                "compress=zstd"
                "noatime"
              ];
            };
            "/media" = {
              mountpoint = "/storage/media";
              mountOptions = [
                "compress=zstd"
                "noatime"
              ];
            };
            "/snapshots" = {
              mountpoint = "/snapshots/storage";
            };
          };
        };
      };
    };
  };
}
