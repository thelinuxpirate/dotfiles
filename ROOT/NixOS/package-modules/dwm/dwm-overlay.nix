{ config, pkgs, ... }: 

{ # Documentation: https://nixos.wiki/wiki/Dwm

  nixpkgs.overlays = [
   (self: super: {
     dwm = super.dwm.overrideAttrs (oldAttrs: rec {
     patches = [
         # Your patches here
	 # If the patch is a local file do:
	 # ./path/to/local.diff
       ];	
     });
   })
 ];
}
