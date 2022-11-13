{
  imports = [
    ../config/options.nix
    ../.
  ];

  custom = {
    desktop-environment = true;
    download-manager = true;
    graphical-applications = true;
    has-yubikey = true;
    is-work = true;
    software-kvm = true;
    is-nixos = true;
  };
}
