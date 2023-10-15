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

    # General
    pkgs.obs-studio
    pkgs.obs-studio-plugins.wlrobs
    pkgs.vlc

    pkgs.blender
    pkgs.gimp
    pkgs.krita

    pkgs.qemu

    # Programming
     # Haskell
    pkgs.ghc
    pkgs.cabal-install
    pkgs.stack

      # Godot + .NET
    pkgs.godot_4
    pkgs.gdtoolkit
    pkgs.pixelorama

    pkgs.dotnet-sdk_8
    
      # Nim
    pkgs.nim
    pkgs.nimlsp

     # Elixir
    pkgs.asdf
    
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

    pkgs.pixelorama
 
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
    pkgs.onefetch
    pkgs.ffmpeg
    
    pkgs.krabby
    pkgs.pipes
  ];

  home.sessionVariables = {
    EDITOR = "emacs";
  };

  services.emacs.enable = true; # XMonad Issues
  programs = {
    kitty = {
      enable = true;
      shellIntegration.enableZshIntegration = true;
      font.name = "Comic Mono";
      font.size = 11;
      theme = "Catppuccin-Macchiato";
    };
    tmux = {
      enable = true;
      tmuxinator.enable = true;

      mouse = true;
      secureSocket = true;
      disableConfirmationPrompt = true;

      historyLimit = 5000;
      escapeTime = 650;
      resizeAmount = 2;
      clock24 = true;
      
      prefix = "C-x";
    };
    emacs = { # Use "Pgtk" for pure GTK | Wayland ONLY, no EXWM
      enable = true;
      package = emacs-overlay.packages.${pkgs.system}.emacs-unstable;
    };
    neovim = {
      enable = true;
      defaultEditor = false;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;
    };
    rofi = {
      enable = true;
      package = pkgs.rofi.override { plugins = [
        pkgs.rofi-systemd
        pkgs.rofi-file-browser
        pkgs.rofi-mpd
        pkgs.rofi-vpn
        pkgs.rofi-calc
      ]; };

      font = "Comic Mono";
      theme = "sidebar";
      location = "center";
    };
  };
  
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  home.stateVersion = "22.11";
}
