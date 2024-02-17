{ config, pkgs, ... }:

{
  home.pointerCursor = {
    package = pkgs.numix-cursor-theme;
    name = "Numix-Cursor-Light";
    size = 20;
    gtk.enable = true;
    x11.enable = true;
  };

  gtk = {
    enable = true;

    theme = {
      name = "Catppuccin-Macchiato-Compact-Pink-Dark";
      package = pkgs.catppuccin-gtk.override {
        accents = [ "pink" ];
        size = "compact";
        tweaks = [ "rimless" "black" ];
        variant = "macchiato";
      };
    };

    iconTheme = {
      name = "ePapirus-Dark";
      package = pkgs.papirus-icon-theme;
    };

    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme=1;
    };
    
    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme=1;
    };
  };
}
