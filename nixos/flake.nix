# flake.nix

{
  inputs = {
  nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  hyprland.url = "github:hyprwm/Hyprland";
  emacs-overlay = {
    url = "github:nix-community/emacs-overlay";
    inputs.nixpkgs.follows = "nixpkgs";
   };
  };

  outputs = {
    self,
    nixpkgs,
    hyprland,
    emacs-overlay,
    ...
  } @inputs:
    let
      emacs-wayland = emacs-overlay.emacsPgkt;
    in {
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
          {
            nixosConfig = {
              environment.systemPackages = [ emacs-wayland ];
            };
          }
          ./hosts/ThePirateShip/configuration.nix
        ];
      };
      KernelCanopy = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/KernelCanopyServer/configuration.nix
        ];
      };
    };
  };
}
