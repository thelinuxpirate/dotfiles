{ config, pkgs, ... }:

# GTK, & Icon Themes for the system:

{ nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
	papirus-icon-theme
        pop-icon-theme
        arc-icon-theme
        tela-icon-theme
        moka-icon-theme
        maia-icon-theme
        kora-icon-theme
        faba-icon-theme
        gruvbox-dark-icons-gtk
        catppuccin-gtk
        ayu-theme-gtk
        flat-remix-gtk
        gruvbox-dark-gtk
        arc-theme
        ant-theme
        yaru-theme
        juno-theme
        qogir-theme
        plata-theme
        canta-theme
        amber-theme
        orchis-theme
        pop-gtk-theme
        oceanic-theme
	materia-theme
        dracula-theme
        spacx-gtk-theme
        paper-gtk-theme
        palenight-theme
	numix-cursor-theme
	nordzy-cursor-theme
	quintom-cursor-theme
	bibata-cursors
	bibata-cursors-translucent
  	phinger-cursors
	openzone-cursors
	capitaine-cursors
  ];
}
