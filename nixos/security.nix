/**
  Configuration for security-related services.
*/
{
  users.users.tlater = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "video"
      "unbound"
    ];
  };

  security = {
    sudo-rs.enable = true;
    rtkit.enable = true;
    pam.enableUMask = true;
  };

  systemd.tmpfiles.settings."10-credstore" = {
    "/etc/credstore".d = {
      user = "root";
      group = "root";
      mode = "0700";
    };

    "/etc/credstore.encrypted".d = {
      user = "root";
      group = "root";
      mode = "0700";
    };
  };
}
