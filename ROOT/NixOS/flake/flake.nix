{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
  };

  outputs = { self, nixpkgs }: {
  };
}
