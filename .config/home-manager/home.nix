{ config, pkgs, ... }:

{
  imports = [ ./desktop/hypr.nix ];
  
  nixpkgs.config.allowUnfreePredicate = _: true;
  
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "pinguino";
  home.homeDirectory = "/home/pinguino";

  home.packages = [
    pkgs.firefox
    pkgs.helix
    pkgs.emacs29-gtk3
    pkgs.neofetch
    pkgs.discord
    pkgs.spotify
    pkgs.dolphin-emu
    pkgs.bottles
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
    package = pkgs.emacs29-gtk3;
  };
  
  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
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
