{ pkgs, nix-gaming, ... }:

{
  home.packages = [
    # Launchers
    pkgs.steam-tui
    pkgs.lutris
    pkgs.grapejuice
    
    # Emulation
    pkgs.yuzu
    pkgs.dolphin-emu
    pkgs.snes9x-gtk
    pkgs.mupen64plus
    pkgs.citra-nightly
    pkgs.vbam

    # Wine
    nix-gaming.packages.${pkgs.system}.wine-ge
    pkgs.bottles
    pkgs.winetricks
  ];
}
