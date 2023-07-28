{ config, pkgs, ... }:

{
  imports = [
    ./pingu/sh.nix
    ./pingu/themes.nix
  ];
  
  nixpkgs.config.allowUnfreePredicate = _: true;
  
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "pinguino";
  home.homeDirectory = "/home/pinguino";

  home.packages = [
    # Web, & Media
    pkgs.firefox
    pkgs.discord
    pkgs.betterdiscordctl
    pkgs.spotify
    pkgs.spicetify-cli

    # Editors & Dev
    pkgs.emacs29-gtk3
    pkgs.helix
    pkgs.neovim

    pkgs.blender

    # Programming
     # Rust
    pkgs.rustup

     # Nim
    pkgs.nim
    pkgs.nimlsp

     # Zig
    pkgs.zig
    pkgs.zls
    
     # Python
    pkgs.python3
    pkgs.python311Packages.pip

     # Go
    pkgs.go
    
     # C / C++
    pkgs.gcc
    pkgs.gnumake
    pkgs.cmake

    # Emulation, & Wine
    pkgs.dolphin-emu
    pkgs.snes9x-gtk
    pkgs.mupen64plus
    pkgs.citra-nightly
    pkgs.vbam
    
    pkgs.bottles
    pkgs.wineWowPackages.stable
    pkgs.winetricks

    # Videos & Streaming
    pkgs.obs-studio
    pkgs.obs-studio-plugins.wlrobs
    pkgs.vlc

    # Gaming
    pkgs.steam
    pkgs.steam-tui

    pkgs.lutris
    
    # Misc
    pkgs.neofetch
    pkgs.krabby
    pkgs.pipes
    pkgs.obsidian

  ];

  home.file = {};

  home.sessionVariables = {
    EDITOR = "emacs";
  };

  # Zonaimacs
  services.emacs = {
    enable = true;
   
  };
  
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  home.stateVersion = "22.11";
}
