{
  config,
  pkgs,
  flake-inputs,
  ...
}:
{
  imports = [
    flake-inputs.home-manager.nixosModules.home-manager
    flake-inputs.sops-nix.nixosModules.sops

    ./dev.nix
    ./greeter
    ./networking
    ./sway.nix
    ./yubikey.nix
  ];

  nix = {
    package = pkgs.nixFlakes;

    settings = {
      auto-optimise-store = true;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      substituters = [ "https://nix-community.cachix.org" ];
      trusted-public-keys = [ "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" ];
    };

    gc = {
      automatic = true;
      dates = "Thu";
    };
  };

  sops = {
    defaultSopsFile = "/etc/sops/secrets.yaml";
    validateSopsFiles = false;
  };

  nixpkgs.overlays = [ flake-inputs.nurpkgs.overlay ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      inherit flake-inputs;
      nixos-config = config;
    };
  };

  boot = {
    tmp.cleanOnBoot = true;
    plymouth.enable = true;
    kernelPackages = pkgs.linuxKernel.packages.linux_xanmod_latest;

    loader = {
      timeout = 0;
      efi.canTouchEfiVariables = true;

      systemd-boot = {
        enable = true;
        configurationLimit = 5;
        editor = false;
      };
    };
  };

  time.timeZone = "Europe/Amsterdam";

  users = {
    defaultUserShell = pkgs.zsh;

    users = {
      tlater = {
        isNormalUser = true;
        extraGroups = [
          "wheel"
          "video"
        ];
      };
    };
  };

  # Ensure that we can find stuff with `man -k` (used with emacs a
  # lot)
  documentation.man.generateCaches = true;

  environment = {
    systemPackages = with pkgs; [ pavucontrol ];

    extraInit = ''
      # Do not want this in the environment. NixOS always sets it and does not
      # provide any option not to, so I must unset it myself via the
      # environment.extraInit option.
      unset -v SSH_ASKPASS
    '';

    pathsToLink = [ "/share/zsh" ];
  };

  programs = {
    dconf.enable = true;
    git.enable = true;
    zsh.enable = true;
    nano.enable = false;
  };

  security.sudo-rs.enable = true;

  fileSystems."/boot".options = [ "umask=0077" ];

  fonts = {
    enableDefaultPackages = true;

    packages = with pkgs; [
      hack-font
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
    ];

    fontconfig = {
      defaultFonts = {
        serif = [ "NotoSerif" ];
        sansSerif = [ "NotoSans" ];
        monospace = [ "Hack" ];
      };
    };
  };

  # My systems never have usable root accounts anyway, so emergency
  # mode just drops into a shell telling me it can't log into root
  systemd.enableEmergencyMode = false;

  services = {
    xserver = {
      enable = true;
      xkb.layout = "us";
    };

    dbus.packages = [
      # Required for gnome3 pinentry to work
      pkgs.gcr
    ];

    libinput = {
      enable = true;
      mouse.middleEmulation = false;
    };

    pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;

      # Disable the HFP bluetooth profile, because I always use external
      # microphones anyway. It sucks and sometimes devices end up caught
      # in it even if I have another microphone.
      wireplumber.extraConfig = {
        "50-bluez" = {
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
            ];

            "bluez5.hfphsp-backend" = "none";
          };
        };
      };
    };

    udisks2.enable = true;
    nscd.enableNsncd = true;
    blueman.enable = true;
    chrony.enable = true;
    flatpak.enable = true;
    fstrim.enable = true;
    fwupd.enable = true;
  };

  hardware = {
    bluetooth.enable = true;
    enableRedistributableFirmware = true;
    opentabletdriver.enable = true;
  };

  security.rtkit.enable = true;

  # Necessary for opening links in gnome under certain conditions
  services.gvfs.enable = true;

  system.stateVersion = "20.09";
}
