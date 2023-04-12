# User settings, applications and preferences
{ config, pkgs, ... }:

let
  home-manager = builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/master.tar.gz";
in
{
  imports = [
    (import "${home-manager}/nixos")
  ];

  users.users.tlp = {
    isNormalUser = true;
    description = "The Linux Pirate";
    extraGroups = [ 
      "networkmanager" 
      "wheel" 
      "audio" 
      "video" 
      "docker" 
      ];
    };

    # Session variables
    environment.sessionVariables = rec {
      EDITOR          = "nvim";
      NIXOS_OZONE_WL  = "1";
    };

    # Disable sudo password prompt
    security = {
      sudo.wheelNeedsPassword = false;
    };

    # Change default shell to fish
    programs.zsh.enable = true;
    users.defaultUserShell = pkgs.zsh;

    # User applications
    environment.systemPackages = with pkgs; [
      brave
      neofetch
      discord
      neovim
      emacs
      spotify
      btop
      obs-studio
      oh-my-zsh 
      steam

      git
      gcc
      rustc
      cargo
      clang
      gnumake
    ];

    programs.zsh.ohMyZsh = {
    enable = true;
    plugins = [ "git" "python" "man" ];
    theme = "agnoster";
  };


    home-manager.users.tlp = {
      home.stateVersion = "18.09";
    };
}
