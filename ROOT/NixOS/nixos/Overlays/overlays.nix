{ config, pkgs, lib, ... }:

{
    # Suckless Slstatus
    nixpkgs.overlays = [
      (final: prev: {
        slstatus = final.slstatus.overrideAttrs (_: {
          src = /home/larry/Suckless/Slstatus;
        });
      })
    ];

    # Emacs-Git from Community Overlay
    nixpkgs.overlays = [
      (import (builtins.fetchTarball {
        url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
      }))
    ];
}
