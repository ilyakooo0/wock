{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    vere = {
      url = "github:urbit/vere?ref=master";
      flake = false;
    };
    urcrypt = {
      url = "github:urbit/urcrypt";
      flake = false;
    };
    murmur3 = {
      url = "github:PeterScott/murmur3";
      flake = false;
    };
    libaes_siv = {
      url = "github:dfoxfranke/libaes_siv";
      flake = false;
    };
    pdjson = {
      url = "github:skeeto/pdjson";
      flake = false;
    };
    softfloat = {
      url = "github:ucb-bar/berkeley-softfloat-3";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, vere, murmur3, pdjson
    , softfloat, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages = rec {
          gmp = pkgs.runCommand "gmp" {
            buildInputs = with pkgs; [ gnutar emscripten gcc ];
          } ''
            tar xvf ${pkgs.gmp.src}
            cd "gmp-${pkgs.gmp.version}"
            mkdir -p $out
            HOME=$TMPDIR
            emconfigure ./configure --disable-assembly --host none HOST_CC=gcc --prefix=$out
            make
            make install
          '';
          openssl = pkgs.openssl.overrideDerivation (old: {
            configurePhase = ''
              HOME=$TMPDIR
              ${pkgs.emscripten}/bin/emconfigure ./Configure --no-asm --no-apps --prefix=$out ${
                builtins.concatStringsSep " " old.configureFlags
              }
            '';
            doCheck = false;
          });
          secp256k1 = pkgs.secp256k1.overrideDerivation (old: {
            configurePhase = ''
              ./autogen.sh
              HOME=$TMPDIR
              ${pkgs.emscripten}/bin/emconfigure ./configure --prefix=$out ${
                builtins.concatStringsSep " " old.configureFlags
              }
            '';
            doCheck = false;
          });
          libaes_siv = pkgs.runCommand "libaes_siv" {
            buildInputs = with pkgs; [ emscripten ];
          } ''
            mkdir -p $out
            HOME=$TMPDIR
            emcc ${inputs.libaes_siv}/aes_siv.c -I${
              ./libaes_siv/include
            } -o $out/lib/libaes_siv.a
            mkdir -p $out/include
            cp ${inputs.libaes_siv}/aes_siv.h $out/include
          '';
          # libaes_siv = pkgs.stdenv.mkDerivation {
          #   name = "libaes_siv";
          #   src = inputs.libaes_siv;
          #   # configurePhase = ''
          #   #   HOME=$TMPDIR
          #   #   ${pkgs.emscripten}/bin/emconfigure ./configure --prefix=$out
          #   # '';
          #   buildInputs = with pkgs; [ cmake openssl ];
          # };
          urcrypt = pkgs.stdenv.mkDerivation {
            name = "urcrypt";
            src = inputs.urcrypt;
            configurePhase = ''
              runHook preConfigure

              ./autogen.sh
              HOME=$TMPDIR
              emconfigure ./configure --prefix=$out

              runHook preConfigure
            '';
            buildInputs = with pkgs; [
              secp256k1
              pkg-config
              libaes_siv
              openssl
            ];
            nativeBuildInputs = with pkgs; [
              emscripten
              autoconf
              automake
              autoconf-archive
              libtool
            ];
          };
          nock = pkgs.runCommand "nock" { } ''
            cp -r ${vere}/pkg .
            chmod -R a+w pkg
            cp pkg/noun/platform/linux/rsignal.h pkg/noun/platform/rsignal.h
            patch -p1 <${./vere.patch}
            rm pkg/noun/*_tests.c
            mkdir -p $out
            HOME=$TMPDIR
            ${pkgs.emscripten}/bin/emcc \
              pkg/noun/*.c pkg/noun/jets/*/*.c pkg/noun/jets/*.c pkg/c3/*.c pkg/ur/*.c ${urcrypt}/lib/liburcrypt.a \
              ${murmur3}/murmur3.c ${pdjson}/pdjson.c ${gmp}/lib/libgmp.a ${urcrypt}/lib/liburcrypt.a \
              -o $out/nock.js \
              -DU3_OS_linux \
              -DU3_OS_ENDIAN_little \
              -DU3_GUARD_PAGE \
              -sEXPORTED_RUNTIME_METHODS=cwrap \
              -Ipkg -Ipkg/noun -Ipkg/c3 -Ipkg/ur -Ipkg/ent -I. \
              -I${pdjson} -I${gmp}/include -I${murmur3} -I${urcrypt}/include -I${pkgs.libsigsegv}/include \
              -I${pkgs.openssl.dev}/include -I${softfloat}/source/include \
              -sEXPORTED_FUNCTIONS=_u3n_nock_on \
              # -Os

          '';
          hello = pkgs.hello;
          default = hello;
        };
        apps = rec {
          hello =
            flake-utils.lib.mkApp { drv = self.packages.${system}.hello; };
          default = hello;
        };
      });
}
