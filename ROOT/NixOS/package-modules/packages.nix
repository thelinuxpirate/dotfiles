{ config, pkgs, ... }:

{ # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  
  # Allows broken packages:
  nixpkgs.config.allowBroken = true;

  # Specify Packages Here:
  environment.systemPackages = with pkgs; [
        vim
        neovim
        emacs
	doas
	zsh
	oh-my-zsh
	fuse
	appimage-run
	ripgrep
	fd
	git
	wget
        links2
        killall
	gnumake
	tree
        plocate
        mlocate
        htop
	neofetch
        pfetch
        ipfetch
	libnotify
	dunst
	alacritty
	easyeffects
	gnome.file-roller
	gnome.gnome-tweaks
	xfce.xfce4-screenshooter
	obs-studio
	obs-studio-plugins.obs-vkcapture
	ranger
	feh
	etcher
	transmission-gtk
	transmission-remote-gtk
	chromium
	spotify
	spotify-tui
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
	gcc
        ghc
	go
        lua5_4
        cargo
	rustc
	rust-analyzer
	rustup
	openssl
	dotnet-sdk
	dotnet-runtime
	dotnet-aspnetcore
	python3
	openssh
	ssh-tools
	ssh-chat
	ssh-ident
	ssh-audit
	xorg.xbacklight
	brightnessctl
	SDL2
	imagemagick
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
	dmenu
	swaybg
	i3lock
	nitrogen
	lxappearance
	libsForQt5.qt5ct
	haskellPackages.haskell-language-server
        haskellPackages.hoogle
        haskellPackages.gtk
	cabal-install
        stack
	haskellPackages.zlib
	zlib
	gnatcoll-zlib
  ];

  nixpkgs.config.permittedInsecurePackages = [
                "electron-12.2.3"
              ];
}
