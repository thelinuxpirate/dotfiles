{ config, pkgs, callPackage, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # SYSTEM:
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

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
  networking.hostName = "ThePirateShip";
  networking.networkmanager.enable = true;

  security.doas = {
    enable = true;
    wheelNeedsPassword = false;
  };

  users.users.tlp = {
    isNormalUser = true;
    description = "TheLinuxPirate";
    extraGroups = [ "networkmanager" "wheel" ];
  };
  
  # Set shell to zsh
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;
   
  # Desktop
  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.wayland  = true;

  # Sound & Media
  sound.enable = true;
  hardware.pulseaudio.enable = false;
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
    enable = false;
  };
  
  nix.gc = { # Automatic Garbage Collection
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };
  
  # System Software & Packages
  environment.systemPackages = with pkgs; [
    # Important (System)
    pkgs.git
    pkgs.home-manager
    pkgs.curl
    pkgs.appimage-run

    # Desktop Dependencies
    pkgs.alacritty
    pkgs.waybar
    pkgs.swaybg
    pkgs.dunst
    pkgs.wofi
    pkgs.xfce.thunar
    pkgs.gnome.file-roller
    pkgs.pavucontrol
    pkgs.sway-contrib.grimshot

    # &Othr
    pkgs.pfetch
    pkgs.btop
    pkgs.tree

    pkgs.wget
    pkgs.lxappearance-gtk2
  ];
  
  fonts.packages = with pkgs; [ # Waybar Dependencies
    pkgs.font-awesome
    pkgs.nerdfonts
  ];
  
  # Daemons, Services, & Programs;
  services.xserver.libinput.enable = true;

  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  system.stateVersion = "23.05";
}
