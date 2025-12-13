{
  config,
  pkgs,
  lib,
  flake-inputs,
  ...
}:
{
  disabledModules = [ "services/system/automatic-timezoned.nix" ];

  imports = [
    flake-inputs.self.nixosModules.nvidia

    flake-inputs.home-manager.nixosModules.home-manager
    flake-inputs.sops-nix.nixosModules.sops
    flake-inputs.nix-flatpak.nixosModules.nix-flatpak

    "${flake-inputs.nixpkgs-tlater}/nixos/modules/services/system/automatic-timezoned.nix"

    ./desktop
    ./dev.nix
    ./networking
    ./yubikey.nix
  ];

  nix = {
    settings = {
      auto-optimise-store = true;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
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

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      inherit flake-inputs;
      nixos-config = config;
    };
  };

  easyNvidia = {
    # Apparently xanmod isn't available for LTS at the moment, oh well
    advanced.forceKernel = true;
    desktopEnvironment = "wlroots";
  };

  hardware.nvidia =
    let
      inherit (flake-inputs.self.packages.${pkgs.stdenv.hostPlatform.system}) nvidia;
    in
    {
      package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
        inherit (nvidia) version;
        sha256_64bit = nvidia.src.outputHash;
        openSha256 = nvidia.open.src.outputHash;
        useSettings = false;
        usePersistenced = false;
      };

      # Disabled because I don't use it and I can't be bothered to
      # figure out how to get a hash for something nvidia don't seem to
      # publish consistently.
      nvidiaSettings = false;
    };

  boot = {
    initrd.systemd.enable = true;

    tmp.cleanOnBoot = true;
    plymouth.enable = true;
    kernelPackages = lib.mkMerge [
      (lib.mkIf config.easyNvidia.enable pkgs.linuxKernel.packages.linux_xanmod_stable)
      (lib.mkIf (!config.easyNvidia.enable) pkgs.linuxKernel.packages.linux_xanmod_latest)
    ];

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

  time.timeZone = lib.mkDefault "Asia/Hong_Kong";

  i18n = {
    supportedLocales = [
      "en_US.UTF-8/UTF-8"
      "en_DK.UTF-8/UTF-8"
    ];

    extraLocaleSettings = {
      LC_MESSAGES = "en_US.UTF-8";
      LC_TIME = "en_DK.UTF-8";
    };
  };

  users = {
    defaultUserShell = pkgs.dash;

    users = {
      tlater = {
        isNormalUser = true;
        extraGroups = [
          "wheel"
          "video"
          "unbound"
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
  };

  programs = {
    dconf.enable = true;
    git.enable = true;
    nano.enable = false;
  };

  security.sudo-rs.enable = true;

  fileSystems."/boot".options = [ "umask=0077" ];

  fonts = {
    enableDefaultPackages = true;

    packages = with pkgs; [
      hack-font
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-color-emoji
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

  systemd.services.angrr-touch = {
    description = "`touch` gcroots that angrr shouldn't delete";
    wantedBy = [ "angrr.service" ];
    before = [ "angrr.service" ];

    serviceConfig.ExecStart =
      (flake-inputs.self.pkgs-lib.${pkgs.stdenv.hostPlatform.system}.writeNuWith
        { packages = [ pkgs.fd ]; }
        "angrr-touch"
        ''
          let roots = (fd --no-ignore -t d gcroots /home/tlater/.local/src /home/tlater/Documents/Projects
             | split row "\n"
             | each { ls -l $in }
             | flatten
             | where target =~ ^/nix/store)

          print ($roots | select name accessed modified | update name { path relative-to /home/tlater })
          $roots | each { touch --no-deref $in.name }
        ''
      ).outPath;
  };

  services = {
    angrr = {
      enable = true;
      enableNixGcIntegration = true;
    };

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
              "sbc"
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
    automatic-timezoned.enable = true;
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
