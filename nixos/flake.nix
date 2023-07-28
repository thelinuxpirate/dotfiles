# flake.nix

{
  inputs = {
  nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  hyprland.url = "github:hyprwm/Hyprland";
  };

  outputs = {
    nixpkgs,
    hyprland,
    ...
  } @inputs: {
    nixosConfigurations.TheTreeHouse = nixpkgs.lib.nixosSystem {
      specialArgs = { inherit inputs; }; # this is the important part
      modules = [
        hyprland.nixosModules.default
        {programs.hyprland.enable = true;}
        ./configuration.nix
      ];
    };
  }; 
}
