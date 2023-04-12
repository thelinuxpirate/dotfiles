{ config, pkgs, ... }:
let
  # Define user in ./username.nix
  # User should have corrsponding [user].nix file
  user = import ./user/username.nix;
in
{
  imports = 
    [ 
      ./hardware-configuration.nix
      ./system/base.nix
      ./user/${user}.nix 
      
];

  networking.hostName = "thepirateship";
  networking.networkmanager.enable = true;  

  time.timeZone = "America/Los_Angeles";
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

  services.openssh.enable = true;
  services.emacs.enable = true;

  nixpkgs.config.allowUnfree = true;
  nix = {
    autoOptimiseStore = true;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc = { # gc = Garbage Collector
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 15d";
    };
  };
  
  system.autoUpgrade = {
    enable = true;
    channel = "https://nixos.org/channels/nixos";
  };

  environment.systemPackages = with pkgs; [
    nix-diff
    git
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Read this bigass comment ^^^
}

