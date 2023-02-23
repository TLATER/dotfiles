{config, ...}: {
  networking.wireless = {
    environmentFile = config.sops.secrets.wireless-env.path;

    networks = {
      mikan = {
        hidden = true;
        authProtocols = ["WPA-PSK"];
        pskRaw = "@PSK_MIKAN@";
      };
    };
  };

  sops.secrets.wireless-env = {};
}
