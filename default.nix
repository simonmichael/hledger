{
  # Get latest commits from https://status.nixos.org/
  # Compute sha256 with `nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/8eaee110344796db060382e15d3af0a9fc396e0e.tar.gz`
  nixpkgs ? fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/8eaee110344796db060382e15d3af0a9fc396e0e.tar.gz";
    sha256 = "1k5zyljqg8fp99hzgjkvkawhmbwlfsnz4viisfcfdjyky9zrc8c8";
  },
  pkgs ? import nixpkgs {
    config = {
      allowUnfree = true;
    };
  },
}:
{
  shell = pkgs.mkShellNoCC {
    packages = with pkgs; [
      bash
      cabal-install
      dateutils
      difftastic
      fd
      gh
      ghc
      ghcid
      hlint
      jj
      just
      pngquant
      stack

      haskellPackages.ghcitui
      haskellPackages.hasktags
      haskellPackages.profiterole
      haskellPackages.profiteur
      haskellPackages.quickbench
      haskellPackages.shelltestrunner
    ];
  };
}
