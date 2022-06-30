{
  description = "NIXOS configuration";
  
  inputs.nixpkgs.url = "nixpkgs/nixos-22.05";
  inputs.unstable.url = "nixpkgs/nixos-unstable";
  inputs.home-manager.url = "github:rycee/home-manager/release-22.05";
  inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nur.url = "github:nix-community/NUR";
  inputs.nur.inputs.nixpkgs.follows = "nixpkgs";

  outputs = inputs@{ self, home-manager, nixpkgs, unstable, nur }:
    let
      overlay-unstable = final: prev: {
        unstable = unstable.legacyPackages.x86_64-linux;
      };
    in
    {      
      homeManagerConfigurations = {
        "kaptch@laptop" = home-manager.lib.homeManagerConfiguration {          
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            overlays = [ nur.overlay overlay-unstable ];
          };
          configuration = { pkgs, config, unstable, ... }:
            let
              overlay-unstable = final: prev: {
                unstable = inputs.unstable.legacyPackages.x86_64-linux;
              };
            in
              {
                nixpkgs.overlays = [ overlay-unstable ];
                imports = [ ./home.nix ];
              };
          system = "x86_64-linux";
          homeDirectory = "/home/kaptch";          
          username = "kaptch";          
          stateVersion = "22.05";
        };
      };
      kaptch = self.homeConfigurations."kaptch@gmail.com".activationPackage;
      defaultPackage.x86_64-linux = self.kaptch;
    };
}
