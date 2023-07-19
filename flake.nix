{
  description = "HLedger via stacklock2nix";

  # This is a flake reference to the stacklock2nix repo.
  #
  # Note that if you copy the `./flake.lock` to your own repo, you'll likely
  # want to update the commit that this stacklock2nix reference points to:
  #
  # $ nix flake lock --update-input stacklock2nix
  #
  # You may also want to lock stacklock2nix to a specific release:
  #
  # inputs.stacklock2nix.url = "github:cdepillabout/stacklock2nix/v1.5.0";
  inputs.stacklock2nix.url = "github:cdepillabout/stacklock2nix/main";

  # This is a flake reference to Nixpkgs.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";

  outputs = { self, nixpkgs, stacklock2nix }:
    let
      # System types to support.
      supportedSystems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor =
        forAllSystems (system: import nixpkgs { inherit system; overlays = [ stacklock2nix.overlay self.overlay ]; });
    in
    {
      # A Nixpkgs overlay.
      overlay = import nix/overlay.nix;

      packages = forAllSystems (system: {
        hledger     = nixpkgsFor.${system}.hledger;
        hledger-web = nixpkgsFor.${system}.hledger-web;
        hledger-ui  = nixpkgsFor.${system}.hledger-ui;
      });

      defaultPackage = forAllSystems (system: self.packages.${system}.hledger);

      devShells = forAllSystems (system: {
        hledger-dev-shell = nixpkgsFor.${system}.hledger-dev-shell;
      });

      devShell = forAllSystems (system: self.devShells.${system}.hledger-dev-shell);
    };
}
