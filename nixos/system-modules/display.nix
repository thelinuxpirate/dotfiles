{ config, pkgs, ... }:

{ # Enables the X11 windowing system.
  services.xserver.enable = true;

  # Window Managers & Desktop Enviorments go here
  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.dwm.enable = true;

  # If you want a display manager enabled set the values here:
  services.xserver.displayManager.gdm.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;
}
