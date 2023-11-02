{
  description = "TheLinuxPirate's NixOS System Manager Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    disko = { # TODO: Set up nixos-anywhere
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    agenix = { # SSH Encryption
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.darwin.follows = "";
    };

    sops-nix = { # Encrypt Files
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-index = { # Search for 'file' via nixpkg
      url = "github:nix-community/nix-index";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ssbm-nix = { # Super Smash Brothers Melee Slippi via NixOS
      url = "github:djanatyn/ssbm-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    hyprland.url = "github:hyprwm/Hyprland";
  };

  outputs = {
    self,
    nixpkgs,
    disko,
    agenix,
    sops-nix,
    nix-index,
    ssbm-nix,
    hyprland,
    ...
  } @inputs: {
      nixosConfigurations = {
      TheTreeHouse = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
          disko.nixosModules.disko
          agenix.nixosModules.default
          sops-nix.nixosModules.sops
          ssbm-nix.nixosModule

          hyprland.nixosModules.default
          {programs.hyprland.enable = true;}

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
