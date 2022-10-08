{ config, pkgs, ... }:

{ # Specify Packages Here:
  environment.systemPackages = with pkgs; [
        vim
        neovim
        emacs
        vscode
        doas
        fish
	zsh
	fuse
        git
	wget
        links2
        tree
        plocate
        mlocate
        neofetch
        pfetch
        ipfetch
        alacritty
	terminator
	st
	gnome.file-roller
        opera
        discord
        blender
        gimp
        steam
        lutris
        pacman                  # Package Manager (Needed for Wii Development);
        gcc
        ghc
        go
        lua5_4
        waybar
        rofi-wayland
        dmenu-wayland
        dolphin-emu
        snes9x
        snes9x-gtk
        mupen64plus
        vbam
        citra-canary
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
}
