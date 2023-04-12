{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    hyprland.url = "github:hyprwm/Hyprland";
    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
};

  outputs = { nixpkgs, hyprland, ... }: {
    nixosConfigurations.thepirateship = nixpkgs.lib.nixosSystem {
      modules = [
        hyprland.nixosModules.default
        { programs.hyprland.enable = true; }
      ];
    };
  };
}
