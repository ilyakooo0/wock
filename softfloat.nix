{ pkgs, softfloat, ... }:
pkgs.runCommand "nock" { buildInputs = [ pkgs.emscripten pkgs.binutils ]; } ''
  mkdir -p $out/lib
  mkdir -p $out/include
  cp -r ${softfloat} src
  chmod +w -R src
  cd src
  patch -p1 <${./softfloat/patch.diff}
  cd build/Linux-x86_64-GCC
  HOME=$TMPDIR
  make
  cp softfloat.a $out/lib
  cp ${softfloat}/source/include/* $out/include
''
