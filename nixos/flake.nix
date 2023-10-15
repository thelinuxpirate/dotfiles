# flake.nix

{
  inputs = {
  nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
    ...
  } @inputs: {
      nixosConfigurations = {
      TheTreeHouse = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/TheTreeHouse/configuration.nix
        ];
      };
      ThePirateShip = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
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
