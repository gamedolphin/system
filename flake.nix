# WARNING : This file was generated by README.org
# DO NOT MODIFY THIS FILE!
# Any changes made here will be overwritten.
{
  description = "Sandeep's nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    sops-nix.url = "github:Mic92/sops-nix";
  };

  outputs = {
    nixpkgs,
    home-manager,
    emacs-overlay,
    sops-nix,
    ...
  }:
    let
      user = import ./user.nix;
      lib = nixpkgs.lib;
      machines = [
        "smallbox"
      ];
      pkgs = import nixpkgs {
        inherit (user) system;
      };
    in
      {
        nixosConfigurations = builtins.listToAttrs (
          builtins.map (machine: {
            name = machine;
            value = lib.nixosSystem {
              modules = [
                ({ ... }: {
                  nixpkgs.overlays = [ emacs-overlay.overlays.default ];
                })
                ./machines/${machine}/configuration.nix
                home-manager.nixosModules.home-manager
                {
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = true;
                  home-manager.extraSpecialArgs = {
                    inherit user;
                  };
                  home-manager.backupFileExtension = "backup";
                  home-manager.users.${user.username} = {
                    imports =  [./machines/${machine}/home.nix];
                  };
                }
                sops-nix.nixosModules.sops  # sops
              ];
              specialArgs = {
                hostname = machine;
                inherit user;
              };
            };
          }) machines
        );
        devShells.${user.system}.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            nil               # nix lsp server
            nixfmt-rfc-style  # nix formatter
            sops              # used to edit secrets
          ];
        };
      };
}
