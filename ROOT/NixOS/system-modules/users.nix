{ config, pkgs, ... }:

{ # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.thelinuxpirate = {
    isNormalUser = true;
    description = "The Linux Pirate";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
    ];
  };
}
