{
  disko.devices = {
    disk = {
      sda = {
        type = "disk";
        device = "/dev/sda";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              start = "1MiB";
              end = "128MiB";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "defaults" ];
              };
            };

            main = {
              start = "128MiB";
              end = "-4G";
              content = {
                type = "luks";
                name = "main";
                settings = {
                  keyFile = "/tmp/secret.key";
                  allowDiscards = true;
                };
                content = {
                  type = "btrfs";
                  extraArgs = [ "-f" ];
                  subvolumes = {
                    "/root" = {
                      mountpoint = "/";
                      mountOptions = [ "compress=zstd" ];
                    };
                    "/persist" = {
                      mountOptions = [ "compress=zstd" ];
                    };
                    "/persist/state" = {
                      mountOptions = [ "compress=zstd" ];
                    };
                    "/persist/data" = {
                      mountOptions = [ "compress=zstd" ];
                    };
                    "/nix" = {
                      mountOptions = [
                        "compress=zstd"
                        "noatime"
                        "noxattr"
                        "noacl"
                      ];
                    };
                  };
                };
              };
            };

            swap = {
              start = "-4G";
              end = "100%";
              content = {
                type = "luks";
                name = "swap";
                settings = {
                  keyFile = "/tmp/secret.key";
                  allowDiscards = true;
                };
                content.type = "swap";
              };
            };
          };
        };
      };
    };
  };
}
