{ stdenv, glibc, autoPatchelfHook }:
stdenv.mkDerivation {
  name = "sphinx-pronounce";
  src = ./.;
  buildInputs = [ glibc ];
  nativeBuildInputs = [ autoPatchelfHook ];
  installPhase = ''
    mkdir -p $out/bin
    autoPatchelf ./pronounce
    cp pronounce $out/bin
    cp quick_lm.pl $out/bin/bin
    mkdir -p $out/share
    cp -r dict lexdata $out/share
  '';
}
