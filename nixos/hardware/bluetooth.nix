/**
  Bluetooth-related configuration.
*/
{
  hardware.bluetooth.enable = true;

  services = {
    blueman.enable = true;

    # TODO: Configure this in the user service via hjem
    #
    # Disable the HFP bluetooth profile, because I always use external
    # microphones anyway. It sucks and sometimes devices end up caught
    # in it even if I have another microphone.
    pipewire.wireplumber.extraConfig."50-bluez" = {
      "monitor.bluez.rules" = [
        {
          matches = [ { "device.name" = "~bluez_card.*"; } ];
          actions = {
            update-props = {
              "bluez5.auto-connect" = [
                "a2dp_sink"
                "a2dp_source"
              ];
              "bluez5.hw-volume" = [
                "a2dp_sink"
                "a2dp_source"
              ];
            };
          };
        }
      ];
      "monitor.bluez.properties" = {
        "bluez5.roles" = [
          "a2dp_sink"
          "a2dp_source"
          "bap_sink"
          "bap_source"
        ];

        "bluez5.codecs" = [
          "ldac"
          "aptx"
          "aptx_ll_duplex"
          "aptx_ll"
          "aptx_hd"
          "opus_05_pro"
          "opus_05_71"
          "opus_05_51"
          "opus_05"
          "opus_05_duplex"
          "aac"
          "sbc_xq"
          "sbc"
        ];

        "bluez5.hfphsp-backend" = "none";
      };
    };
  };
}
