{
  imports = [
    ../.
  ];

  custom = {
    desktop-environment = false;
    download-manager = false;
    graphical-applications = false;
    has-yubikey = false;
    # Actually, it is usually work, but the configuration means something else
    # TODO(tlater): Find a better name for this config option
    is-work = false;
    software-kvm = false;
    is-nixos = false;
  };
}
