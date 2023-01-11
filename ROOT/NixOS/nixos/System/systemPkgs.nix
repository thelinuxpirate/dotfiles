{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
        neovim 
        wget
        git
        doas
	      zsh
	      killall
	      gnumake
	      tree
        alacritty
	      easyeffects
	      gnome.file-roller
	     	gcc
        ghc
	      go
        lua5_4
        cargo
	      rustc
	      rust-analyzer
	      rustup
	      openssl
	      python3
	      openssh
	      ssh-tools
	      ssh-chat
	      ssh-ident
	      ssh-audit
	      xorg.xbacklight
	      xorg.xorgserver
	      xorg.xf86inputevdev
	      xorg.xf86inputsynaptics
	      xorg.xf86inputlibinput
	      xorg.xf86videointel
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
        pamixer
  ];

  nixpkgs.config.permittedInsecurePackages = [
                "electron-12.2.3"
              ];
}
