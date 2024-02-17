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
    hostName = "ThePirateBay";
    networkmanager.enable = true; 
  };
  
  # User Settings & Generation
  users = {
    defaultUserShell = pkgs.zsh;
    users.trong = {
      isNormalUser = true;
      description = "TRONG";
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
        pkgs.clinfo
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

     displayManager.startx.enable = true; # Backup Xorg Support
     displayManager.session = [
       {
         manage = "window";
         name = "EXWM";
         start = ''
          emacsclient -c 
          '';
       }
     ];
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
    hyprland = {
      enable = true;
      package = pkgs.hyprland;
      portalPackage = pkgs.xdg-desktop-portal-hyprland;
      xwayland.enable = true;
    };
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
  
  # Fonts
  fonts = {
    packages = with pkgs; [ # Desktop Font Dependencies 
      pkgs.comic-mono
      pkgs.font-awesome
      pkgs.nerdfonts
      pkgs.minecraftia
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
    pkgs.wezterm
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
    pkgs.waybar
    pkgs.wofi
    pkgs.swaybg
    pkgs.mpvpaper
    pkgs.xfce.xfce4-screenshooter
    pkgs.eww-wayland

    # EXWM | Xorg
    # pkgs.polybar
    # pkgs.feh
    # pkgs.flameshot

    # Desktop Dependencies
    pkgs.dunst
    pkgs.playerctl
    pkgs.brightnessctl

    pkgs.xfce.thunar
    pkgs.xfce.thunar-volman
    pkgs.xfce.thunar-dropbox-plugin
    pkgs.xfce.thunar-archive-plugin
    pkgs.xfce.thunar-media-tags-plugin
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
