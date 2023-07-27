{ config, pkgs, callPackage, ... }:

{
  imports = [ ./desktop/hypr.nix ];
  
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

  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  home.sessionVariables = {
    EDITOR = "emacs";
  };
  
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
    
  services.emacs = {
    defaultEditor = true;
    package = pkgs.emacs-unstable-pgtk; # emacs29-gtk3
  };
  
  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    initExtra = "krabby random -i --no-title\n";
    oh-my-zsh = {
      enable = true;
      theme = "afowler";
    };
    shellAliases = {
      d = "doas";
      s = "sudo";

      c = "clear";
      t = "tree";
      vi = "nvim";

      melee = "cd && ./System/Applications/Slippi/Slippi-Launcher.AppImage && cd -";
      discordBot = "cd && ./System/Code/wiggler/target/release/wiggler && cd -";
    };
  };
  
  home.stateVersion = "22.11";
}
