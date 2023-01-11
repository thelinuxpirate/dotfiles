{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ./System/systemConfig.nix
      ./System/systemPkgs.nix
      ./User/userConfig.nix
      ./User/userPkgs.nix
    ];

    system.stateVersion = "22.11"; 
}
