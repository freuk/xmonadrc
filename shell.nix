with import ./. { };

haskellPackages.shellFor {
  packages = p: [ haskellPackages.xmonadrc ];
  withHoogle = true;
  buildInputs = [
    ghcid
    dhall
    haskellPackages.dhall-to-cabal
    haskellPackages.panpipe
    haskellPackages.panhandle
    cabal2nix
    ormolu
    hlint
    pandoc
    cabal-install
    lmstuff
  ];
}
