_: let
  disks = [
    "/dev/sda"
  ];
in {
  disko.devices = {
    disk = {
      sda = {
        type = "disk";
        device = builtins.elemAt disks 0;
        content = {
          type = "table";
          format = "gpt";
          partitions = [
            {
              name = "ESP";
              start = "1MiB";
              end = "128MiB";
              fs-type = "fat32";
              bootable = true;
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            }
            {
              name = "main";
              start = "128MiB";
              end = "-4G";
              content = {
                type = "luks";
                name = "main";
                extraOpenArgs = ["--allow-discards"];
                keyFile = "/tmp/secret.key";
                content = {
                  type = "btrfs";
                  extraArgs = ["-f"]; # Override existing partition
                  subvolumes = {
                    "/root" = {
                      mountpoint = "/";
                      mountOptions = ["compress=zstd"];
                    };
                    "/persist" = {
                      mountOptions = ["compress=zstd"];
                    };
                    "/persist/state" = {
                      mountOptions = ["compress=zstd"];
                    };
                    "/persist/data" = {
                      mountOptions = ["compress=zstd"];
                    };
                    "/nix" = {
                      mountOptions = ["compress=zstd" "noatime" "noxattr" "noacl"];
                    };
                  };
                };
              };
            }
            {
              name = "swap";
              start = "-4G";
              end = "100%";
              content = {
                type = "luks";
                name = "swap";
                extraOpenArgs = ["--allow-discards"];
                keyFile = "/tmp/secret.key";
                content = {
                  type = "swap";
                };
              };
            }
          ];
        };
      };
    };
  };
}
