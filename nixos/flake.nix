# flake.nix

{
  inputs = {
  nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  hyprland.url = "github:hyprwm/Hyprland";
  };

  outputs = {
    self,
    nixpkgs,
    hyprland,
    ...
  } @inputs: {
      nixosConfigurations = {
      TheTreeHouse = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
          hyprland.nixosModules.default
          {programs.hyprland.enable = true;}
          ./hosts/TheTreeHouse/configuration.nix
        ];
      };
      ThePirateShip = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
          hyprland.nixosModules.default
          {programs.hyprland.enable = true;}
          ./hosts/ThePirateShip/configuration.nix
        ];
      };
      ThePirateCove = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/KernelCanopyServer/configuration.nix
        ];
      };
    };
  };
}
