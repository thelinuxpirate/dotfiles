{ config, pkgs, ... }:

{ networking.hostName = "thepirateship";
  # networking.wireless.enable = true;  # <= Wpa_Supplicant

  # Netowrk Proxy:
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  #  Networking with NetworkManager:
  networking.networkmanager.enable = true;
}
