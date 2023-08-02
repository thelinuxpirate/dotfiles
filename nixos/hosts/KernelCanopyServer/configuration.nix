{ config, pkgs, ... }:
# Try to keep the server as minimal as possible. We don't want bloat;
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
  networking.hostName = "KernelCanopy";
  networking.networkmanager.enable = true;

  users.users.grimace = {
    isNormalUser = true;
    description = "Maintainer";
    extraGroups = [ "networkmanager" "wheel" ];
  };
   
  # Graphics
  services.xserver.enable = true;
  services.xserver.displayManager.startx.enable = true;
  services.xserver.windowManager.dwm.enable = true;
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  hardware.opengl = {
    enable = true;
    driSupport = true;
  };
  
  # Nix Settings
  nixpkgs.config.allowUnfree = false;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  
  # System Software & Packages
  environment.systemPackages = with pkgs; [
    # Important (System)
    pkgs.git
    pkgs.curl
    
    # Grahical Environment
    pkgs.st
    pkgs.dmenu

    # &Othr
    pkgs.btop
    pkgs.tree
  ];
  
  # Daemons, Services, & Programs;
  services.openssh.enable = true;
  services.udisks2.enable = true;
  programs.neovim.enable = true; # You'll need a text editor at least;
  
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  
  system.stateVersion = "23.05";
}
