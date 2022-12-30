# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ ./system-modules/boot.nix
      ./hardware-configuration.nix
      ./system-modules/users.nix
      ./system-modules/home-manager/home-manager.nix
      ./package-modules/packages.nix
      ./system-modules/audio.nix
      ./system-modules/network.nix
      ./system-modules/display.nix
      ./package-modules/visual.nix
      ./package-modules/hyprland.nix
      ./package-modules/dwm/dwm-overlay.nix
    ];

  # Enables Flakes:
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  
  # Set default shell:
  users.defaultUserShell = pkgs.zsh;

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.utf8";

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Some programs need SUID wrappers, can be configured further or are started in user sessions.
    programs.mtr.enable = true;
    
    security.sudo.wheelNeedsPassword = false;
    security.doas.wheelNeedsPassword = false;

    security.doas.enable = true;
    security.doas.extraRules = [{
    groups = [ "wheel" ];
    users = [ "thelinuxpirate" ];
    keepEnv = true;
    }];

  # 32 Bit Support for OpenGL:
  hardware.opengl.driSupport32Bit = true;

  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
