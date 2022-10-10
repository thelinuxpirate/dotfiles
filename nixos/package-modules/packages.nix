{ config, pkgs, ... }:

{ # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  
  # Specify Packages Here:
  environment.systemPackages = with pkgs; [
        vim
        neovim
        doas
	zsh
	fuse
        git
	wget
        links2
        tree
        plocate
        mlocate
        htop
	neofetch
        pfetch
        ipfetch
	alacritty
	gnome.file-roller
	gnome.gnome-tweaks
	xfce.xfce4-screenshooter
	ranger
	feh
	chromium
	spotify
        discord
	vlc
        blender
        gimp
        steam
        lutris
        gcc
        ghc
        go
        lua5_4
        xorg.xbacklight
	vulkan-loader
	vulkan-tools
	vulkan-headers
	vulkan-tools-lunarg
	wine
	wine64
	waybar
        xmobar
	picom
	wofi
	wofi-emoji
	rofi-wayland
        dmenu-wayland
	swaybg
	nitrogen
	lxappearance
	libsForQt5.qt5ct
  ];
}
