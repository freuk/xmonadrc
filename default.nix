{ pkgs ? import (builtins.fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz") { } }:
with pkgs.lib;

let
  ormolu = let
    source = pkgs.fetchFromGitHub {
      owner = "tweag";
      repo = "ormolu";
      rev = "f83f6fd1dab5ccbbdf55ee1653b24595c1d653c2";
      sha256 = "1hs7ayq5d15m9kxwfmdac3p2i3s6b0cn58cm4rrqc4d447yl426y";
    };
  in (import source { }).ormolu;

in pkgs // rec {
  lmstuff = pkgs.pkgsi686Linux.callPackage ./vendor { };

  dhall-to-cabal-resources = pkgs.stdenv.mkDerivation {
    name = "dhall-to-cabal-resources";
    src = pkgs.haskellPackages.dhall-to-cabal.src;
    installPhase = "cp -r dhall $out";
  };

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super:
      with pkgs.haskell.lib; rec {
        xmonadrc = pkgs.haskellPackages.callCabal2nix "xmonadrc"./. {};
        refined = unmarkBroken super.refined;
        dhall-to-cabal = unmarkBroken super.dhall-to-cabal;
        lazysmallcheck2012 = null;
      };
  };
  inherit ormolu;

  hlint = haskellPackages.hlint;
  xmonadrc = haskellPackages.xmonadrc;
}
