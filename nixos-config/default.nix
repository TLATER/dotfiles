{
  config,
  pkgs,
  lib,
  flake-inputs,
  ...
}:
{
  imports = [
    flake-inputs.self.nixosModules.nvidia

    flake-inputs.home-manager.nixosModules.home-manager
    flake-inputs.sops-nix.nixosModules.sops
    flake-inputs.stylix.nixosModules.stylix
    flake-inputs.nix-flatpak.nixosModules.nix-flatpak

    ./desktop
    ./dev.nix
    ./networking
    ./yubikey.nix
  ];

  nix = {
    package = pkgs.lix;

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
      inherit (flake-inputs.self.packages.${pkgs.system}) nvidia;
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

  stylix = {
    enable = true;
    autoEnable = false;

    # OKSolar (https://meat.io/oksolar)
    base16Scheme = {
      # Background tones
      base00 = "#002d38"; # Background
      base01 = "#093946"; # Lighter background (status bars)

      # Content tones
      ## TODO(tlater): Fix these colors; they're not completely suited
      ## ATM because solarized considers these "content colors", while
      ## base16 only considers base03 a content color
      base02 = "#5b7279"; # Selection background
      base03 = "#657377"; # Comments, invisbles, line highlighting
      base04 = "#98a8a8"; # Dark foreground
      base05 = "#8faaab"; # Default foreground

      # Background tones
      base06 = "#f1e9d2"; # Light foreground
      base07 = "#fbf7ef"; # Lightest foreground

      # Accent colors
      base08 = "#f23749";
      base09 = "#d56500";
      base0A = "#ac8300";
      base0B = "#819500";
      base0C = "#259d94";
      base0D = "#2b90d8";
      base0E = "#7d80d1";
      base0F = "#dd459d";

      # Emphasis accent - these aren't in the upstream color scheme,
      # but I'm taking the idea from the "emphasis" text color of
      # going with more saturated varaints of the normal accent
      # colors.
      #
      # TODO(tlater): Need to pick better colors here
      base10 = config.stylix.base16Scheme.base01; # Darker background
      base11 = config.stylix.base16Scheme.base00; # Darkest background
      base12 = "#ff0034";
      base13 = "#b48000";
      base14 = "#809600";
      base15 = "#00a297";
      base16 = "#008ff2";
      base17 = "#7873ff";
    };

    # cursor = {};
    # fonts = {};
  };

  boot = {
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
