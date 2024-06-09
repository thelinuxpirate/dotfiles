{
  description = "Home-Manager config for Non-NixOS Distros";

  inputs = {
    # Specify the source of Nixpkgs, get Flake-Utils, & Home-Manager:
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Spicetify
    spicetify-nix = {
      url = "github:the-argus/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
  self, 
  nixpkgs,
  flake-utils,
  home-manager,
  spicetify-nix,
  ... 
  }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      specialArgs = {
        inherit spicetify-nix;
      }; in { 
        homeConfigurations.trong = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = specialArgs;
          modules = [
            ./home.nix
          ];
        };
    };
}
