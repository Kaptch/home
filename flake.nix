{
  description = "NIXOS configuration";

  inputs.nixpkgs.url = "nixpkgs/nixos-22.11";
  inputs.nixpkgs-master.url = "github:NixOS/nixpkgs/master";
  inputs.unstable.url = "nixpkgs/nixos-unstable";
  inputs.home-manager.url = "github:rycee/home-manager/release-22.11";
  inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nur.url = "github:nix-community/NUR";
  # inputs.nur.inputs.nixpkgs.follows = "nixpkgs";
  inputs.lean4.url = "github:leanprover/lean4";
  inputs.coq-lsp = { type = "git"; url = "https://github.com/ejgallego/coq-lsp"; submodules = true; };

  outputs = inputs@{ self, home-manager, nixpkgs, unstable, nur, lean4, coq-lsp, nixpkgs-master }:
    let
      overlays = final: prev: {
        unstable = unstable.legacyPackages.x86_64-linux;
        lean4pkg = inputs.lean4.outputs.defaultPackage.x86_64-linux;
        coq-lsp-pkg = inputs.coq-lsp.outputs;
        master = nixpkgs-master.legacyPackages.x86_64-linux;
      };
    in
    {
      homeManagerConfigurations = {
        "kaptch@laptop" = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            overlays = [ nur.overlay overlays ];
          };
          modules = [
            ./home.nix
            {
              home = {
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
