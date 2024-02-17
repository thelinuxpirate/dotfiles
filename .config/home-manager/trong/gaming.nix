{ pkgs, nix-gaming, nix-minecraft, ... }:

{
  home.packages = [
    # Launchers
    pkgs.steam-tui
    pkgs.lutris
    pkgs.minecraft
    pkgs.grapejuice
    
    # Emulation
    pkgs.yuzu
    pkgs.dolphin-emu
    pkgs.snes9x-gtk
    pkgs.mupen64plus
    pkgs.citra-nightly
    pkgs.vbam

    # Wine
    nix-gaming.packages.${pkgs.system}.proton-ge
    nix-gaming.packages.${pkgs.system}.northstar-proton
    nix-gaming.packages.${pkgs.system}.wine-ge
    pkgs.bottles
    pkgs.winetricks
  ];
}
