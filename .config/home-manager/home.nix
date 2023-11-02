{ config, pkgs, emacs-overlay, ... }:

{
  imports = [
    ./pingu/sh.nix
    ./pingu/themes.nix
    ./pingu/spicetify.nix
    ./pingu/gaming.nix
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

    pkgs.tor-browser-bundle-bin
    pkgs.transmission-gtk
    pkgs.obs-studio
    pkgs.obs-studio-plugins.wlrobs
    pkgs.vlc

    # General
    pkgs.blender
    pkgs.gimp
    pkgs.krita
    pkgs.qemu

    pkgs.krabby
    pkgs.neofetch
    pkgs.onefetch
    pkgs.ffmpeg
    pkgs.xdelta
    pkgs.hyprpicker
    
    # Programming
    pkgs.ghc # Haskell
    pkgs.cabal-install
    pkgs.stack
    pkgs.godot_4 # Godot/C#
    pkgs.gdtoolkit
    pkgs.pixelorama
    pkgs.dotnet-sdk_8
    pkgs.nim # Nim
    pkgs.nimlsp
    pkgs.asdf # Elixir 
    pkgs.zig # Zig
    pkgs.zls
    pkgs.sbcl # Lisp/Scheme
    pkgs.guile_3_0
    pkgs.python311Packages.pip # Python
    pkgs.go # Golang
    pkgs.gcc # C/C++
    pkgs.gnumake
    pkgs.cmake
    pkgs.lua # Lua
    pkgs.nodejs # JS/TS
    pkgs.bun
  ];

  home.sessionVariables = {
    EDITOR = "emacs";
  };

  services.emacs.enable = true; # Has issues on XMonad
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
    emacs = { # Use "Pgtk" for pure GTK | Wayland ONLY; No EXWM
      enable = true;
      package = emacs-overlay.packages.${pkgs.system}.emacs-pgtk;
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
  
  # Home Manager required
  programs.home-manager.enable = true;
  home.stateVersion = "22.11";
}
