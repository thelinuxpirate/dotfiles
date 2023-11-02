{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # System
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  
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

  # Networking & Pinguino
  networking.hostName = "TheTreeHouse";
  networking.networkmanager.enable = true;

  security.doas = {
    enable = true;
    wheelNeedsPassword = false;
  };

  users.users.pinguino = {
    isNormalUser = true;
    description = "Larry Hamilton";
    extraGroups = [ "networkmanager" "wheel" ];
  };
  
  # Set shell to zsh
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;
   
  # Desktop:
  services.xserver = {
    enable = true;
    updateDbusEnvironment = true;
    
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
  
  # Sound & Media
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  hardware.pulseaudio.support32Bit = true;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    wireplumber.enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # Nix Settings
  nixpkgs.config.allowUnfree = true;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  system.autoUpgrade = { # Auto-Upgrade NixOS System
    enable = true;
  };
  
  nix.gc = { # Automatic Garbage Collection
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
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

  # Fonts
  fonts = {
    packages = with pkgs; [ # Desktop & Snormacs Fonts
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
  
  # Gaming
  programs.steam.enable = true;
  programs.gamemode.enable = true;
  services.joycond.enable = true;
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = [ pkgs.mesa.drivers ];
  };

  ssbm = { # Melee for NixOS
    overlay.enable = true;
    cache.enable = true;
    gcc = {
      oc-kmod.enable = true;
      rules.enable = true;
    };
  };
  
  # Daemons, Services, & Programs;
  services.xserver.libinput.enable = true;
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  services.emacs.enable = true;
  services.openssh.enable = true;
  services.printing.enable = true;
  services.blueman.enable = true;
  services.udisks2 = {
    enable = true;
  };

  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  
  system.stateVersion = "23.05";
}
