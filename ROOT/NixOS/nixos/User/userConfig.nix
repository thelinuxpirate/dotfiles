{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  users.users.larry = {
    isNormalUser  = true;
    home  = "/home/larry";
    description  = "Larry Hamilton";
    extraGroups  = [ "wheel" "networkmanager" ];
    packages = with pkgs; [
      emacs
#      pkgs.emacsGit
      (import /etc/nixos/User/userEmacs.nix { inherit pkgs; })
      brave
      oh-my-zsh
      zsh-syntax-highlighting
	    fuse
	    appimage-run
	    ripgrep
	    fd
      links2
      neofetch
      pfetch
      ipfetch
	    libnotify
	    dunst
	    xfce.xfce4-screenshooter
	    obs-studio
	    obs-studio-plugins.obs-vkcapture
	    ranger
	    feh
	    etcher
	    transmission-gtk
	    transmission-remote-gtk
	    spotify
	    spotify-tui
      pavucontrol
	    discord
	    betterdiscordctl
	    vlc
      amberol
	    blender
      gimp
      steam
      lutris
      godot
	    lmms
      slstatus
      wine
	    wine64
	    picom
	    dmenu
	    nitrogen
	    lxappearance
    ];
  };
}
