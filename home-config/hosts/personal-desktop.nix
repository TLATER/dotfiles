{
  imports = [
    ../.
  ];

  custom = {
    desktop-environmnet.enable = true;
    download-manager.enable = true;
    graphical-applications.enable = true;
    has-yubikey = true;
    is-work = false;
    software-kvm = true;
  };
}
