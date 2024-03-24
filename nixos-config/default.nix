{
  config,
  pkgs,
  flake-inputs,
  ...
}: {
  imports = [
    flake-inputs.home-manager.nixosModules.home-manager
    flake-inputs.sops-nix.nixosModules.sops

    ./dev.nix
    ./greeter
    ./networking
    ./sway.nix
    ./yubikey.nix
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
    defaultSopsFile = "/etc/sops/secrets.yaml";
    validateSopsFiles = false;
  };

  nixpkgs.overlays = [
    flake-inputs.nurpkgs.overlay
  ];

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
        extraGroups = ["wheel" "video"];
      };
    };
  };

  # Ensure that we can find stuff with `man -k` (used with emacs a
  # lot)
  documentation.man.generateCaches = true;

  environment = {
    systemPackages = with pkgs; [
      pavucontrol
    ];

    extraInit = ''
      # Do not want this in the environment. NixOS always sets it and does not
      # provide any option not to, so I must unset it myself via the
      # environment.extraInit option.
      unset -v SSH_ASKPASS
    '';

    pathsToLink = [
      "/share/zsh"
    ];

    # Disable the HFP bluetooth profile, because I always use external
    # microphones anyway. It sucks and sometimes devices end up caught
    # in it even if I have another microphone.
    etc."wireplumber/bluetooth.lua.d/50-bluez-config.lua".text = ''
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
  };

  programs = {
    dconf.enable = true;
    git.enable = true;
    zsh.enable = true;
    nano.enable = false;
  };

  security.sudo-rs.enable = true;

  fileSystems."/boot".options = ["umask=0077"];

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
