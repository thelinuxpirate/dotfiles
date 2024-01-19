{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # NixOS-Boot
  boot.loader = { 
    timeout = 25;
    systemd-boot = { 
      enable = true;
      configurationLimit = 80;
      netbootxyz.enable = true;
     };
    efi.canTouchEfiVariables = true;
  };

  # Set Desktop Hostname
  networking = { 
    hostName = "ThePirateCove";
    networkmanager.enable = true; 
  };
  
  # User Settings & Generation
  users = {
    defaultUserShell = pkgs.zsh;
    users.thelinuxpirate = {
      isNormalUser = true;
      description = "Larry Hamilton";
      extraGroups = [ "networkmanager" "wheel" ];
    };
  };
  # Locales & Time Zone
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };
  time.timeZone = "America/Los_Angeles";

  # NixOS Settings
  system = {
    autoUpgrade = {
      enable = true;
      allowReboot = false;
      channel = "https://channels.nixos.org/nixos-unstable";
      operation = "boot";
    };
  };

  # Configure Nix
  nix = {
    gc = { # Automatic Garbage Collection
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
    settings.experimental-features = [ "nix-command" "flakes" ];
  };

  nixpkgs.config.allowUnfree = true;
  
  # Hardware Settings
  sound.enable = true;
  hardware = {
    bluetooth = { 
      enable = true;
      powerOnBoot = true;
     };
    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [ 
        pkgs.mesa.drivers  
	      pkgs.amdvlk
	      pkgs.clinfo
      ];
      extraPackages32 = with pkgs; [ 
	      pkgs.driversi686Linux.amdvlk
      ];
    };
  };
  
  # Services & Daemons
  services = {
    # Display
    xserver = {
      enable = true;
      updateDbusEnvironment = true;
      videoDrivers = [ "amdgpu" ];
      libinput.enable = true;
      layout = "us";
      xkbVariant = "";
    
     displayManager.gdm = {
        enable = true;
        autoSuspend = false;
        wayland  = true;
      };

      displayManager.startx.enable = true; # Error Checking (in case Hyprland has errors)
      windowManager.xmonad = { 
        enable = true;
        enableContribAndExtras = true;
      };
    };
    
    # Audio
    pipewire = {
      enable = true;
      wireplumber.enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
    };
    
    # Other Services
    printing.enable = true;
    openssh.enable = true;
    joycond.enable = true;
    blueman.enable = true;
    udisks2.enable = true;
  };

  # Desktop Programs
  programs = {
    zsh.enable = true;
    steam.enable = true; 
    gamemode.enable = true;
    mtr.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };
  
  # Security Options
  security = {
    sudo = {
      execWheelOnly = true;
      wheelNeedsPassword = true;
    };
    doas = {
      enable = true;
      wheelNeedsPassword = false;
    };
    rtkit.enable = true;
  };
  
  # SSBM Flake Settings (Melee for NixOS)
  ssbm = {
    overlay.enable = true;
    cache.enable = true;
    gcc = {
      oc-kmod.enable = true;
      rules.enable = true;
    };
  };

  # Fonts
  fonts = {
    packages = with pkgs; [ # Desktop Font Dependencies 
      pkgs.comic-mono
      pkgs.font-awesome
      pkgs.nerdfonts
    ];
    fontDir.enable = true;
    fontconfig = {
      defaultFonts = {
        serif = [ "Comic Mono" ];
        sansSerif = [ "Comic Mono" ];
        monospace = [ "Comic Mono" ];
      };
    };
  };

  # System Software & Packages
  environment.systemPackages = with pkgs; [
    # Important (System)
    pkgs.kitty
    pkgs.git
    pkgs.home-manager
    pkgs.curl

    pkgs.appimage-run
    pkgs.pciutils
    pkgs.usbutils
    pkgs.glxinfo
    pkgs.vulkan-tools
    
    # Graphical Desktop
     # Hyprland | Wayland
    pkgs.eww-wayland
    pkgs.waybar
    pkgs.wofi
    pkgs.swaybg
    pkgs.mpvpaper
    pkgs.sway-contrib.grimshot

     # XMonad | Xorg
    # pkgs.polybar
    # pkgs.feh
    # pkgs.picom-jonaburg
    # pkgs.flameshot

    # Desktop Dependencies
    pkgs.dunst
    pkgs.playerctl
    pkgs.brightnessctl

    pkgs.xfce.thunar
    pkgs.gnome.file-roller
    pkgs.bluetuith
    pkgs.blueberry
    pkgs.pavucontrol

    # &Othr
    pkgs.pfetch
    pkgs.btop
    pkgs.tree
  ];

  system.stateVersion = "23.05";
}
