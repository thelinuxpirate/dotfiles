{ config, pkgs, ... }:

{ # X Server Configuration:

services.xserver = {
    enable = true;
  
    displayManager = {
        gdm.enable = true;
    };
    
    desktopManager.gnome.enable = true;
    windowManager.dwm.enable = true;
    windowManager.awesome = {
      enable = true;
      luaModules = with pkgs.luaPackages; [
        luarocks # is the package manager for Lua modules
        luadbi-mysql # Database abstraction layer
      ];
    };
    windowManager = {
    xmonad = {
    enable = true;
    enableContribAndExtras = true;
    extraPackages = haskellPackages: [
      haskellPackages.dbus
      haskellPackages.List
      haskellPackages.monad-logger
      haskellPackages.xmonad
      haskellPackages.xmonad-contrib
     ];
   };
 };
    # Touchpad suport for some systems (Most of the time its enabled by default):
    libinput.enable = true;

  };
}
