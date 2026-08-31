{ lib, ... }: {
  home.file.".ssh/tlater.pub".text = ''
    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMMst2rs9WuvWnRTOuQElDMx0/cf4n9x9lC1+8clT0LZ openpgp:0xDD46BD5E
  '';

  programs.ssh = {
    enableDefaultConfig = false;
    settings = {
      "*".IdentityFile = "~/.ssh/tlater.pub";

      "tlater.net" = lib.hm.dag.entryAfter [ "*" ] {
        HostName = "tlater.net";
        User = "tlater";
        Port = 2222;
        ForwardAgent = true;
      };

      "console.gl-inet.com" = lib.hm.dag.entryAfter [ "*" ] {
        HostName = "console.gl-inet.com";
        User = "root";
        PubkeyAcceptedAlgorithms = "+ssh-rsa";
        HostkeyAlgorithms = "+ssh-rsa";
      };

      "hetzner-1" = lib.hm.dag.entryAfter [ "*" ] {
        HostName = "116.202.158.55";
        User = "tlater";
        Port = 2222;
        ForwardAgent = true;
      };
    };
  };
}
