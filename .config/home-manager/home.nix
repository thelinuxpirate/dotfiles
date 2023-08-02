{ config, pkgs, lib, ... }:

{
  imports = [
    ./pingu/sh.nix
    ./pingu/spicetify.nix
    ./pingu/themes.nix
  ];
  
  nixpkgs.config.allowUnfreePredicate = _: true;
  
  # Home Manager needs a bit of information about you and the paths it should manage.
  home.username = "pinguino";
  home.homeDirectory = "/home/pinguino";

  home.packages = [
    # Web, & Media
    pkgs.firefox
    pkgs.discord
    pkgs.betterdiscordctl

    # Editors & Dev
    pkgs.helix
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
    pkgs.yuzu
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

  services.emacs.enable = true;

  programs.emacs = { 
    enable = true; 
    package = pkgs.emacs29-gtk3;
  };

  programs.neovim = {
    enable = true;
    defaultEditor = false;

    viAlias = true;
    vimAlias = true;
  };
  

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  home.stateVersion = "22.11";
}
