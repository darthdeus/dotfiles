{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }@attrs: {
    # packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;
    # packages.x86_64-linux.default = self.packages.x86_64-linux.hello;

    defaultPackage.x86_64-linux = home-manager.defaultPackage.x86_64-linux;

    homeConfigurations = {
      "darth" = home-manager.lib.homeManagerConfiguration {
         pkgs = import nixpkgs { system = "x86_64-linux"; };

         modules = [ ./home.nix ];
      };
    };

    nixosConfigurations.pipik = {
      system = "x86_64-linux";
      # specialArgs = attrs;
      modules = [ ./configuration.nix ];
    };
  };
}
