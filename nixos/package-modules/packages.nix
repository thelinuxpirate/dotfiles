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
	etcher
	chromium
	spotify
        discord
	vlc
        blender
        gimp
        steam
        lutris
        lmms
	gcc
        ghc
        go
        lua5_4
        openssh
	ssh-tools
	ssh-chat
	ssh-ident
	ssh-audit
	xorg.xbacklight
	vulkan-loader
	vulkan-tools
	vulkan-headers
	vulkan-tools-lunarg
	gtk4
	gtk3
	gtk2
	gtk3-x11
	gtk2-x11
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
	haskellPackages.haskell-language-server
        haskellPackages.hoogle
        haskellPackages.gtk
	cabal-install
        stack
  ];

  nixpkgs.config.permittedInsecurePackages = [
                "electron-12.2.3"
              ];
}
