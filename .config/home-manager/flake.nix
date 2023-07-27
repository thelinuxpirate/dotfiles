{
  description = "Home Manager configuration of pinguino";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    }; 
    hyprland.url = "github:hyprwm/Hyprland";
  };

  outputs = { 
  nixpkgs, 
  home-manager, 
  hyprland, 
  ... 
  }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      homeConfigurations.pinguino = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
        hyprland.homeManagerModules.default
        {wayland.windowManager.hyprland.enable = true;} 

        ./home.nix 
        ];
      };
    };
}
