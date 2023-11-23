{
  lib,
  config,
  pkgs,
  flake-inputs,
  ...
}: {
  imports = [
    flake-inputs.home-manager.nixosModules.home-manager
    flake-inputs.sops-nix.nixosModules.sops

    ./greeter
    ./wireguard.nix
    ../modules
  ];

  nix = {
    package = pkgs.nixFlakes;

    settings = {
      auto-optimise-store = true;
      experimental-features = ["nix-command" "flakes"];
      substituters = [
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };

    gc = {
      automatic = true;
      dates = "Thu";
    };

    # Make the nixpkgs flake input be used for various nix commands
    nixPath = ["nixpkgs=${flake-inputs.nixpkgs}"];
    registry.nixpkgs = {
      from = {
        id = "nixpkgs";
        type = "indirect";
      };
      flake = flake-inputs.nixpkgs;
    };
  };

  sops = {
    gnupg = {
      home = "/var/lib/sops";
      sshKeyPaths = [];
    };

    defaultSopsFile = "/etc/sops/secrets.yaml";
    validateSopsFiles = false;
  };

  nixpkgs.overlays = [
    flake-inputs.nurpkgs.overlay
  ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs.flake-inputs = flake-inputs;
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

  networking = {
    wireless = {
      enable = true;

      allowAuxiliaryImperativeNetworks = true;
      userControlled = {
        enable = true;
        group = "network";
      };
    };
    useDHCP = false;
    useNetworkd = true;
  };
  systemd.network.wait-online.anyInterface = true;

  time.timeZone = "Europe/Amsterdam";

  users = {
    defaultUserShell = pkgs.zsh;

    groups.network = {};

    users = {
      tlater = {
        isNormalUser = true;
        extraGroups = ["wheel" "video" "network"];
      };
    };
  };

  # Ensure that we can find stuff with `man -k` (used with emacs a
  # lot)
  documentation.man.generateCaches = true;

  environment.systemPackages = with pkgs; [
    git # To manage the nixos configuration, all users need git
    home-manager # To manage the actual user configuration
    pavucontrol # In case the host doesn't have audio, this can't be in the user config
    wpa_supplicant_gui # For managing wireless networks
    bibata-cursors

    firefox
  ];

  theming.cursor.theme = "Bibata-Original-Ice";

  environment.extraInit = ''
    # Do not want this in the environment. NixOS always sets it and does not
    # provide any option not to, so I must unset it myself via the
    # environment.extraInit option.
    unset -v SSH_ASKPASS
  '';

  environment.pathsToLink = [
    "/share/zsh"
  ];

  programs = {
    dconf.enable = true;
    zsh.enable = true;
    hyprland = {
      enable = true;
      package = lib.mkDefault flake-inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.hyprland;
    };
  };

  # Override the default xdg portal set by the hyprland module
  # TODO(tlater): Starting with 23.11 there will be an option for this
  xdg.portal.extraPortals = lib.mkForce [
    flake-inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.xdg-desktop-portal-hyprland
  ];

  security.pam.services.swaylock = {};

  fonts = {
    enableDefaultFonts = true;

    fonts = with pkgs; [
      hack-font
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
    ];

    fontconfig = {
      defaultFonts = {
        serif = ["NotoSerif"];
        sansSerif = ["NotoSans"];
        monospace = ["Hack"];
      };
    };
  };

  # My systems never have usable root accounts anyway, so emergency
  # mode just drops into a shell telling me it can't log into root
  systemd.enableEmergencyMode = false;

  services = {
    xserver = {
      enable = true;
      layout = "us";
      libinput = {
        enable = true;
        mouse.middleEmulation = false;
      };
    };

    pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
    };

    udev.packages = [pkgs.yubikey-personalization];

    nscd.enableNsncd = true;
    blueman.enable = true;
    chrony.enable = true;
    pcscd.enable = true;
    flatpak.enable = true;
    fstrim.enable = true;
    fwupd.enable = true;
  };

  # Disable the HFP bluetooth profile, because I always use external
  # microphones anyway. It sucks and sometimes devices end up caught
  # in it even if I have another microphone.
  environment.etc."wireplumber/bluetooth.lua.d/50-bluez-config.lua".text = ''
    bluez_monitor.enabled = true

    bluez_monitor.properties = {
      ["bluez5.headset-roles"] = "[ ]",
      ["bluez5.hfphsp-backend"] = "none",
    }

    bluez_monitor.rules = {
      {
        matches = {
          {
            -- This matches all cards.
            { "device.name", "matches", "bluez_card.*" },
          },
        },

        apply_properties = {
          -- Auto-connect device profiles on start up or when only partial
          -- profiles have connected. Disabled by default if the property
          -- is not specified.
          --["bluez5.auto-connect"] = "[ hfp_hf hsp_hs a2dp_sink hfp_ag hsp_ag a2dp_source ]",
          ["bluez5.auto-connect"]  = "[ a2dp_sink a2dp_source ]",

          -- Hardware volume control (default: [ hfp_ag hsp_ag a2dp_source ])
          --["bluez5.hw-volume"] = "[ hfp_hf hsp_hs a2dp_sink hfp_ag hsp_ag a2dp_source ]",
          ["bluez5.hw-volume"] = "[ a2dp_sink a2dp_source ]",

          -- LDAC encoding quality
          -- Available values: auto (Adaptive Bitrate, default)
          --                   hq   (High Quality, 990/909kbps)
          --                   sq   (Standard Quality, 660/606kbps)
          --                   mq   (Mobile use Quality, 330/303kbps)
          --["bluez5.a2dp.ldac.quality"] = "auto",

          -- AAC variable bitrate mode
          -- Available values: 0 (cbr, default), 1-5 (quality level)
          --["bluez5.a2dp.aac.bitratemode"] = 0,

          -- Profile connected first
          -- Available values: a2dp-sink (default), headset-head-unit
          --["device.profile"] = "a2dp-sink",

          -- Opus Pro Audio encoding mode: audio, voip, lowdelay
          --["bluez5.a2dp.opus.pro.application"] = "audio",
          --["bluez5.a2dp.opus.pro.bidi.application"] = "audio",
        },
      },
    }
  '';

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
