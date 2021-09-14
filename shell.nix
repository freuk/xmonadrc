with import ./. { };

haskellPackages.shellFor {
  packages = p: [ xmonadrc ];
  withHoogle = true;
  buildInputs = [
    ghcid
    dhall
    cabal2nix
    ormolu
    hlint
    pandoc
    cabal-install
    lmstuff
  ];
}
