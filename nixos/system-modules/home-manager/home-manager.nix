{ config, pkgs, ... }:

{
  imports = [ <home-manager/nixos> ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  home-manager.useGlobalPkgs = true;

  home-manager.users.thelinuxpirate = { pkgs, ... }: {

    home.packages = with pkgs; [
	vscode
	fish
	doas
	cowsay
	terminator
	st
	kitty
	tmux
	pacman
	nodejs
	xfce.thunar
	opera
	brave
	tor
        firefox
	dolphin-emu
        snes9x
        snes9x-gtk
        mupen64plus
        vbam
        citra-canary	
	];
  }; 
}
