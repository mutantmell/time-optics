{ pkgs ? import <nixpkgs> {} }:

let
  ghc = pkgs.haskell.packages.ghc92.ghcWithHoogle (hps: [
    hps.optics
    hps.text_2_0_2
    hps.bytestring
  ]);
in pkgs.mkShell {
  packages = [
    # keep this line if you use bash
    pkgs.bashInteractive

    pkgs.cabal-install
    pkgs.haskell-ci
    ghc
    pkgs.haskellPackages.fourmolu
  ];
}
