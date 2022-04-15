{
  description = "NIXOS configuration";
  
  inputs.nixpkgs.url = "nixpkgs/nixos-21.11";
  inputs.unstable.url = "nixpkgs/nixos-unstable";
  inputs.home-manager.url = "github:rycee/home-manager/release-21.11";
  inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nur.url = "github:nix-community/NUR";
  inputs.nur.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, home-manager, nixpkgs, unstable, nur }: 
    {
      homeManagerConfigurations = {
        "kaptch@laptop" = home-manager.lib.homeManagerConfiguration {
          configuration = { pkgs, nixpkgs, unstable, nur, lib, ... }: { imports = [ ./home.nix ]; };
          system = "x86_64-linux";
          homeDirectory = "/home/kaptch";
          username = "kaptch";
          stateVersion = "21.11";
          extraSpecialArgs = { inherit nixpkgs unstable nur; };
        };
      };
    };
}
