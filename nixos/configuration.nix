{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
#      ./home-manager/home.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "TheTreeHouse"; # Define your hostname.

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Enable Usage of Flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  # Set shell to zsh
  programs.zsh.enable = true;

 #  Select internationalisation properties.
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

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  security.doas = {
    enable = true;
    wheelNeedsPassword = false;
  };

  # Desktop
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.wayland  = true;
  programs.hyprland.enable = true; 
  services.dbus.enable = true;
  xdg.portal.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

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
    #media-session.enable = true;
  };
  services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.pinguino = {
    isNormalUser = true;
    description = "Larry Hamilton";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs; [
  pkgs.home-manager
  
    pkgs.alacritty
    pkgs.helix
    pkgs.tree
    pkgs.pfetch
    pkgs.wofi 
    pkgs.waybar 

    pkgs.git
    pkgs.curl
    pkgs.appimage-run
    pkgs.pavucontrol
    pkgs.lxappearance
    pkgs.swaybg

    pkgs.gnome.file-roller
    pkgs.dunst

    pkgs.gcc
    pkgs.cmake
    pkgs.gnumake
    pkgs.libtool
    pkgs.libvterm

    pkgs.busybox
    
    pkgs.python3
    pkgs.python3Packages.pip
  ];

  fonts.fonts = with pkgs; [
    pkgs.font-awesome
    pkgs.nerdfonts
    ];

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
  
  services.openssh.enable = true;
  system.stateVersion = "23.05";
}
