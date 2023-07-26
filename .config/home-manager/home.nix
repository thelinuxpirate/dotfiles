{ config, pkgs, ... }:

{
  imports = [ <home-manager/nixos> ];

  # User Config
  users.users.pinguino.shell = pkgs.zsh;
  services.emacs = {
      enable               = true;
      defaultEditor        = true;
  };

  # HOME MANAGER
  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;

  home-manager.users.pinguino = { pkgs, ... }: {
  home.stateVersion = "23.05";
  programs.home-manager.enable = true;

  home.username = "pinguino";
  home.homeDirectory = "/home/pinguino";
  
  home.packages = [
    pkgs.tmux
    pkgs.neofetch
    pkgs.pokemon-colorscripts-mac
    pkgs.emacs-gtk 
    pkgs.firefox
    pkgs.dolphin-emu
    pkgs.blender
    pkgs.discord
    pkgs.betterdiscordctl
    pkgs.spotify
    pkgs.spicetify-cli
    pkgs.steam
    pkgs.lutris
    pkgs.vbam
    pkgs.citra-nightly
    pkgs.btop
  ];  

  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    oh-my-zsh = {
      enable  = true;
      plugins = [ "git" ];
      theme   = "afowler";
    };
    initExtraFirst = "neofetch";
    initExtra = # Add to $PATH
      "export PATH=\"$HOME/.local/bin:$PATH\"\n
       export PATH=\"$HOME/System/Applications/smblevelworkshop2/build/ws2editor/launch/:$PATH\"\n"; 
    localVariables = {
      GRIM_DEFAULT_DIR = [ "~/Pictures/Screenshits/" ];
    };
    shellAliases = {
      s="sudo";
      vi="nvim";
      t="tree";
      c="clear";
      godot="cd && ./System/Applications/Godot4/Godot_v4.0.2-stable_mono_linux.x86_64 && cd -";
      melee="cd && ./System/Applications/Slippi/Slippi-Launcher.AppImage && cd -";
      wiggler=" cd && ./System/Code/thetuss/thetuss && cd -";
    };
  };
    
    fonts.fontconfig.enable = true;
    gtk = {
      enable = true;
      theme = {
        name    = "nordic";
        package = pkgs.nordic;
      };
    };
  };
}