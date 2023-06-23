{ config, pkgs, ... }:

{
  imports = [ <home-manager/nixos> ];

  # User Config
  users.users.tlp.shell = pkgs.zsh;
  services.emacs = {
      enable               = false;
      defaultEditor        = true;
  };

  # HOME MANAGER
  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;

  home-manager.users.tlp = { pkgs, ... }: {
  home.stateVersion = "23.05";
  programs.home-manager.enable = true;

  home.username = "tlp";
  home.homeDirectory = "/home/tlp";
  
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
    pkgs.zoom
    pkgs.steam
    pkgs.lutris
    pkgs.vbam
    pkgs.citra-nightly
  ];  

  programs.zsh = {
    enable = true;
    #defaultKeymap = "vicmd";
    enableSyntaxHighlighting = true;
    oh-my-zsh = {
      enable  = true;
      plugins = [ "git" ];
      theme   = "bira";
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
      tuss=" cd && ./System/Code/thetuss/thetuss && cd -";
      Cfetch="neofetch --ascii '$(cat ~/.local/neofetch/charmander)' ";
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
