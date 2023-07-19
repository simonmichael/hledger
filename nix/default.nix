{...}:

# Nixpkgs with overlays for stacklock2nix.  This is convenient to use with
# `nix repl`:
#
# $ nix repl ./nix
# nix-repl>
#
# Within this nix-repl, you have access to everything defined in ./overlay.nix.

let
  flake-lock = builtins.fromJSON (builtins.readFile ../flake.lock);

  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${flake-lock.nodes.nixpkgs.locked.rev}.tar.gz";
    sha256 = flake-lock.nodes.nixpkgs.locked.narHash;
  };

  # This is the stacklock2nix source code.
  stacklock2nix-src = builtins.fetchTarball {
    url = "https://github.com/cdepillabout/stacklock2nix/archive/${flake-lock.nodes.stacklock2nix.locked.rev}.tar.gz";
    sha256 = flake-lock.nodes.stacklock2nix.locked.narHash;
  };

  overlays = [
    (import (stacklock2nix-src + "/nix/overlay.nix"))
    (import ./overlay.nix)
  ];

  pkgs = import nixpkgs-src { inherit overlays; };

in

pkgs
