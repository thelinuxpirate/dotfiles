{ config, pkgs, emacs-overlay, ... }:

{
  imports = [
    ./pingu/sh.nix
    ./pingu/themes.nix
    ./pingu/spicetify.nix
  ];
  
  nixpkgs.config.allowUnfreePredicate = _: true;
  
  # Home Manager needs a bit of information about you and the paths it should manage.
  home.username = "pinguino";
  home.homeDirectory = "/home/pinguino";

  home.packages = [    
    # Web, & Media
    pkgs.firefox-devedition
    pkgs.discord
    pkgs.betterdiscordctl
    pkgs.grapejuice

    pkgs.tor-browser-bundle-bin
    pkgs.transmission-gtk
    pkgs.playerctl
    
    # General
    pkgs.obs-studio
    pkgs.obs-studio-plugins.wlrobs
    pkgs.vlc

    pkgs.blender
    pkgs.gimp
    pkgs.krita

    # Programming
     # Nim
    pkgs.nim
    pkgs.nimlsp

     # Zig
    pkgs.zig
    pkgs.zls

     # Lisp/Scheme
    pkgs.sbcl
    pkgs.guile_3_0
    
     # Python
    pkgs.python311Packages.pip

     # Go
    pkgs.go
    
     # C / C++
    pkgs.gcc
    pkgs.gnumake
    pkgs.cmake

     # Lua
    pkgs.lua

     # Web
    pkgs.nodejs

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

    # Gaming
    pkgs.steam-tui
    pkgs.lutris
    
    # Misc
    pkgs.neofetch
    pkgs.krabby
    pkgs.pipes

    pkgs.mpvpaper
    pkgs.obsidian
  ];

#  home.file = {};

  home.sessionVariables = {
    EDITOR = "emacs";
  };

  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        term = "xterm-256color";
        font = "Comic Mono:size=11";
        pad = "6x6 center";
        dpi-aware = "yes";
      };
      mouse = { hide-when-typing = "yes"; };

      colors = {
        foreground = "cdd6f4";
        background = "1e1e2e";
        regular0 = "45475a";
        regular1 = "f38ba8";
        regular2 = "a6e3a1";
        regular3 = "f9e2af";
        regular4 = "89b4fa";
        regular5 = "f5c2e7";
        regular6 = "94e2d5";
        regular7 = "bac2de";
        bright0 = "585b70";
        bright1 = "f38ba8";
        bright2 = "a6e3a1";
        bright3 = "f9e2af";
        bright4 = "89b4fa";
        bright5 = "f5c2e7";
        bright6 = "94e2d5";
        bright7 = "a6adc8";
      };
    };
  };
  
  services.emacs.enable = true;   
  programs.emacs = {
    enable = true;
    package = emacs-overlay.packages.${pkgs.system}.emacs-pgtk;
  };
  
  programs.helix = {
    enable = true;
    defaultEditor = false;
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  home.stateVersion = "22.11";
}
