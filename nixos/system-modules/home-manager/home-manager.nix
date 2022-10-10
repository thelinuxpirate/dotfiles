{ config, pkgs, ... }:

{
  imports = [ <home-manager/nixos> ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  home-manager.useGlobalPkgs = true;

  home-manager.users.thelinuxpirate = { pkgs, ... }: {

    home.packages = with pkgs; [
	emacs 
	vscode
	fish
	doas
	cowsay
	terminator
	st
	kitty
	tmux
	pacman
	xfce.thunar
	opera
	brave
	tor
        dolphin-emu
        snes9x
        snes9x-gtk
        mupen64plus
        vbam
        citra-canary	
	];
  }; 
}
