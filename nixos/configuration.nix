# TLP's "thepirateship" NixOS Configuration
{ config, lib, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./home-manager/home.nix
    ];

  # CORE
  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # Define your hostname.
  networking.hostName = "thepirateship"; 
  # Enable networking
  networking.networkmanager.enable = true;
  # Enable Usage of Flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  # Set shell to zsh
  programs.zsh.enable = true;
  # Set your time zone.
  time.timeZone = "America/Los_Angeles";
  # Select internationalisation properties.
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

  # DESKTOP
  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable   = true;
  services.xserver.displayManager.gdm.wayland  = true;
  services.xserver.windowManager.session = lib.singleton {
    name = "Emacs";
    start = ''
      xhost +SI:localuser:$USER
      exec emacs
    ''; };
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  }; security.polkit.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };
  services.dbus.enable = true;
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    # gtk portal needed to make gtk apps happy
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };
  
  # SERVICES & SETTINGS
  # Define user account, using the base API;
  security.sudo.wheelNeedsPassword = false;
  users.users.tlp = {
    isNormalUser = true;
    description = "Larry Hamilton";
    extraGroups = [ "networkmanager" "wheel" ];
  };
  # Enable CUPS to print documents.
  services.printing.enable = true;
  # Enable Flatpak
  services.flatpak.enable = true;
  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
    #media-session.enable = true; # Enabled by default
  };
  # Automatic Garbage Collection
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };
  # Auto-Upgrade NixOS System
  system.autoUpgrade = {
    enable = true;
  };

  # SYSTEM-PACKAGES
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    pkgs.alacritty
    pkgs.neovim
    pkgs.pfetch
    pkgs.tree
    
    pkgs.wofi
    pkgs.pavucontrol
    pkgs.gnome.nautilus
    pkgs.unzip

    pkgs.gnome.file-roller
    pkgs.dunst
    pkgs.playerctl
    pkgs.sway-contrib.grimshot
    pkgs.picom

    pkgs.swaybg
    pkgs.waybar
    pkgs.eww
    pkgs.lxappearance
    pkgs.feh
    pkgs.autotiling

    pkgs.git
    pkgs.curl
    pkgs.appimage-run

    pkgs.wineWowPackages.stable
    pkgs.wineWowPackages.waylandFull
    pkgs.wine
    (wine.override { wineBuild = "wine64"; }) # 64-Bit support
    pkgs.winetricks
    
    pkgs.gcc
    pkgs.cmake
    pkgs.gnumake
    pkgs.libtool
    pkgs.libvterm

    pkgs.python3
    pkgs.python3Packages.pip
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
