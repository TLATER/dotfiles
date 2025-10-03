{ lib, ... }:
{
  home.file.".ssh/tlater.pub".text = ''
    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMMst2rs9WuvWnRTOuQElDMx0/cf4n9x9lC1+8clT0LZ openpgp:0xDD46BD5E
  '';

  programs.ssh.matchBlocks = {
    "*" = {
      identitiesOnly = true;
      identityFile = "~/.ssh/tlater.pub";
    };

    "tlater.net" = lib.hm.dag.entryAfter [ "*" ] {
      hostname = "tlater.net";
      user = "tlater";
      port = 2222;
      forwardAgent = true;
    };

    "console.gl-inet.com" = lib.hm.dag.entryAfter [ "*" ] {
      hostname = "console.gl-inet.com";
      user = "root";
      extraOptions = {
        PubkeyAcceptedAlgorithms = "+ssh-rsa";
        HostkeyAlgorithms = "+ssh-rsa";
      };
    };

    "hetzner-1" = lib.hm.dag.entryAfter [ "*" ] {
      hostname = "116.202.158.55";
      user = "tlater";
      port = 2222;
      forwardAgent = true;
    };
  };
}
