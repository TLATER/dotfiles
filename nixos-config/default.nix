{
  config,
  pkgs,
  flake-inputs,
  ...
}: {
  imports = [
    flake-inputs.home-manager.nixosModules.home-manager
    flake-inputs.peerix.nixosModules.peerix
    flake-inputs.sops-nix.nixosModules.sops
    flake-inputs.hyprland.nixosModules.default

    ./greeter
    ./wireguard.nix
  ];

  nix = {
    package = pkgs.nixFlakes;

    settings = {
      auto-optimise-store = true;
      experimental-features = ["nix-command" "flakes"];
      substituters = ["https://hyprland.cachix.org"];
      trusted-public-keys = [
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        (builtins.readFile ../keys/peerix/yui.pub)
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

  services.peerix = {
    enable = true;
    openFirewall = true;
    user = "peerix";
    group = "peerix";

    # Work around https://github.com/cid-chan/peerix/issues/11
    package = flake-inputs.peerix.packages.${pkgs.system}.peerix;
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
    kernelPackages = pkgs.linuxPackages_latest;

    initrd.luks.devices.root.allowDiscards = true;

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

  fileSystems."/nix".options = ["defaults" "noatime"];

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

  time.timeZone = "Europe/London";

  users = {
    defaultUserShell = pkgs.zsh;

    groups = {
      peerix = {};
      network = {};
    };

    users = {
      tlater = {
        isNormalUser = true;
        extraGroups = ["wheel" "video" "network"];
      };

      peerix = {
        group = "peerix";
        isSystemUser = true;
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

    firefox
  ];

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
      xwayland.hidpi = false;
      nvidiaPatches = true;
    };
  };

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
