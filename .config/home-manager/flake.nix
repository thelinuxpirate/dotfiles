{
  description = "Personal Home Manager Configuration; Created by: TheLinuxPirate";

  inputs = {
    # Specify the source of Nixpkgs, get Flake-Utils, & Home-Manager:
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Emacs-Overlay
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Rust-Overlay
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Spicetify
    spicetify-nix = {
      url = "github:the-argus/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Gaming Tools
    nix-gaming = {
      url = "github:fufexan/nix-gaming";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
  self, 
  nixpkgs,
  flake-utils,
  home-manager,
  emacs-overlay,
  rust-overlay,
  spicetify-nix,
  nix-gaming,
  ... 
  }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      specialArgs = {
        inherit emacs-overlay;
        inherit rust-overlay;
        inherit spicetify-nix;
        inherit nix-gaming;
      }; in { 
        homeConfigurations.pinguino = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = specialArgs;
          modules = [
            ({ pkgs, ... }: { # Rust
              nixpkgs.overlays = [ rust-overlay.overlays.default ];
              home.packages = [ pkgs.rust-bin.beta.latest.default ];
            })
            ./home.nix
          ];
        };
    };
}
