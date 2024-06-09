{ config, pkgs, ... }:

{
  imports = [ ./trong/spicetify.nix ];
  
  nixpkgs.config.allowUnfreePredicate = _: true;
  home.username = "trong";
  home.homeDirectory = "/home/trong";

  home.packages = [    
    pkgs.betterdiscordctl
    pkgs.tor-browser-bundle-bin
    pkgs.onefetch
  ];
  
  programs.home-manager.enable = true;
  home.stateVersion = "22.11";
}
