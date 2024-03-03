{
  config,
  lib,
  flake-inputs,
  ...
}: {
  config = lib.mkIf config.custom.has-yubikey {
    home.file.".ssh/tlater.pub".source = "${flake-inputs.self}/keys/tlater.pub";

    programs.ssh = {
      enable = true;
      matchBlocks = {
        "*" = {
          identitiesOnly = true;
          identityFile = "~/.ssh/tlater.pub";
        };

        "tlater.net" = lib.hm.dag.entryAfter ["*"] {
          hostname = "tlater.net";
          user = "tlater";
          port = 2222;
          forwardAgent = true;
        };

        "console.gl-inet.com" = lib.hm.dag.entryAfter ["*"] {
          hostname = "console.gl-inet.com";
          user = "root";
          extraOptions = {
            PubkeyAcceptedAlgorithms = "+ssh-rsa";
            HostkeyAlgorithms = "+ssh-rsa";
          };
        };

        "hetzner-1" = lib.hm.dag.entryAfter ["*"] {
          hostname = "116.202.158.55";
          user = "tlater";
          port = 2222;
          forwardAgent = true;
        };
      };
    };

    services = {
      gpg-agent = {
        enable = true;
        enableSshSupport = true;
        defaultCacheTtl = 86400;
        maxCacheTtl = 2592000;
      };
    };
  };
}
