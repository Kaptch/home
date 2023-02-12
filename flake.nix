{
  description = "NIXOS configuration";

  inputs.nixpkgs.url = "nixpkgs/nixos-22.11";
  inputs.unstable.url = "nixpkgs/nixos-unstable";
  inputs.home-manager.url = "github:rycee/home-manager/release-22.11";
  inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nur.url = "github:nix-community/NUR";
  inputs.nur.inputs.nixpkgs.follows = "nixpkgs";
  inputs.lean4.url = "github:leanprover/lean4";

  outputs = inputs@{ self, home-manager, nixpkgs, unstable, nur, lean4 }:
    let
      overlay-unstable = final: prev: {
        unstable = unstable.legacyPackages.x86_64-linux;
        lean4pkg = inputs.lean4.outputs.defaultPackage.x86_64-linux;
      };
    in
    {
      homeManagerConfigurations = {
        "kaptch@laptop" = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            overlays = [ nur.overlay overlay-unstable ];
          };
          modules = [
            ./home.nix
            {
              home = {
                # system = "x86_64-linux";
                homeDirectory = "/home/kaptch";
                username = "kaptch";
                stateVersion = "22.11";
              };
            }
          ];
        };
      };
      kaptch = self.homeConfigurations."kaptch@gmail.com".activationPackage;
      defaultPackage.x86_64-linux = self.kaptch;
    };
}
