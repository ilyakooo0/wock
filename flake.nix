{
  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs?ref=4c0bb97b29ea716c4ced5da385e021caaa55c481";
    flake-utils.url = "github:numtide/flake-utils";
    vere = {
      url = "github:urbit/vere?ref=master";
      flake = false;
    };
    murmur3 = {
      url = "github:PeterScott/murmur3";
      flake = false;
    };
    urcrypt = {
      url = "github:urbit/urcrypt?rev=43479c3262a11e20da5f6218f3b0b3d63931ceea";
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
    sha256 = {
      url = "github:LekKit/sha256";
      flake = false;
    };
    libaes_siv = {
      url = "github:dfoxfranke/libaes_siv";
      flake = false;
    };
  };
  outputs =
    inputs@{ self, nixpkgs, flake-utils, vere, murmur3, pdjson, sha256, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = { permittedInsecurePackages = [ "openssl-1.1.1w" ]; };
        };
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
          libsigsegv = pkgs.libsigsegv.overrideDerivation (old: {
            configurePhase = ''
              runHook preConfigure

              HOME=$TMPDIR
              ${pkgs.emscripten}/bin/emconfigure ./configure --prefix=$out ${
                builtins.concatStringsSep " " old.configureFlags
              }

              runHook postConfigure
            '';
            doCheck = false;
          });
          secp256k1 = pkgs.secp256k1.overrideDerivation (old: {
            configurePhase = ''
              runHook preConfigure

              HOME=$TMPDIR
              ${pkgs.emscripten}/bin/emconfigure ./configure --prefix=$out ${
                builtins.concatStringsSep " " old.configureFlags
              }

              runHook postConfigure
            '';
            doCheck = false;
          });
          openssl = pkgs.openssl_1_1.overrideDerivation (old: {
            configurePhase = ''
              runHook preConfigure

              HOME=$TMPDIR
              export CC=${pkgs.emscripten}/bin/emcc
              ./Configure linux-generic64 no-shared -no-asm no-engine no-tests --prefix=$out

              runHook postConfigure
            '';

            postInstall = ''
              mkdir -p $bin
              mv $out/bin $bin/bin

              mkdir $dev
              mv $out/include $dev/
            '';
          });
          libaes_siv = pkgs.runCommand "libaes_siv" {
            buildInputs = with pkgs; [ emscripten ];
          } ''
            mkdir -p $out/lib
            HOME=$TMPDIR
            emcc -c ${inputs.libaes_siv}/aes_siv.c -I${
              ./libaes_siv/include
            } -I${openssl.dev}/include -o $out/lib/aes_siv.o
            mkdir -p $out/include
            cp ${inputs.libaes_siv}/aes_siv.h $out/include/aes_siv.h
          '';
          urcrypt = pkgs.runCommand "urcrypt" {
            nativeBuildInputs = with pkgs; [ emscripten ];
          } ''
            cp -r ${inputs.urcrypt} src
            chmod +w -R src

            rm src/scrypt/main.c

            mkdir -p $out/lib
            mkdir -p $out/include
            HOME=$TMPDIR
            emcc -c -I${openssl.dev}/include -I${libaes_siv}/include \
            src/argon2/src/argon2.c src/argon2/src/blake2/blake2b.c \
            src/argon2/src/core.c src/argon2/src/encoding.c src/argon2/src/thread.c \
            src/argon2/src/ref.c src/ed25519/src/*.c src/ge-additions/ge-additions.c \
            src/keccak-tiny/keccak-tiny.c src/scrypt/*.c \
            src/urcrypt/*.c \
            -I${secp256k1}/include -I${pkgs.libb2}/include -Isrc/argon2/include \
            -Isrc/ed25519/src/ -Isrc/ge-additions -Isrc/keccak-tiny -Isrc/scrypt \
            -Wno-int-conversion
            emar rcs $out/lib/urcrypt.a *.o

            cp src/urcrypt/urcrypt.h $out/include
          '';
          softfloat =
            pkgs.callPackage ./softfloat.nix { inherit (inputs) softfloat; };
          nock = pkgs.runCommand "nock" { } ''
            cp -r ${vere}/pkg .
            chmod -R a+w pkg
            cp pkg/noun/platform/linux/rsignal.h pkg/noun/platform/rsignal.h
            patch -p1 <${./vere.patch}
            rm pkg/noun/*_tests.c
            mkdir -p $out
            HOME=$TMPDIR
            ${pkgs.emscripten}/bin/emcc \
              pkg/noun/*.c pkg/noun/jets/*/*.c pkg/noun/jets/*.c pkg/c3/*.c pkg/ur/*.c \
              pkg/noun/v3/*.c pkg/noun/v2/*.c pkg/noun/v1/*.c pkg/ent/ent.c \
              ${murmur3}/murmur3.c ${pdjson}/pdjson.c ${gmp}/lib/libgmp.a ${sha256}/sha256.c \
              ${softfloat}/lib/softfloat.a ${libsigsegv}/lib/libsigsegv.a ${urcrypt}/lib/urcrypt.a \
              ${openssl.out}/lib/libcrypto.a ${libaes_siv}/lib/aes_siv.o ${secp256k1}/lib/libsecp256k1.a \
              -o $out/nock.js \
              -DU3_OS_linux \
              -DU3_OS_ENDIAN_little \
              -DU3_GUARD_PAGE \
              -DENT_GETENTROPY_SYSRANDOM \
              -sWASM=1 \
              -sEXPORTED_RUNTIME_METHODS=cwrap \
              -sALLOW_MEMORY_GROWTH \
              -sINITIAL_MEMORY=536870912 \
              -Ipkg -Ipkg/noun -Ipkg/c3 -Ipkg/ur -Ipkg/ent -I. \
              -I${pdjson} -I${gmp}/include -I${murmur3} -I${libsigsegv}/include \
              -I${softfloat}/include -I${sha256} -I${urcrypt}/include -I${openssl.dev}/include \
              -sEXPORTED_FUNCTIONS=_u3n_nock_on,_u3m_boot_lite,_u3s_cue_bytes \
              -sEXPORTED_RUNTIME_METHODS=cwrap \
              # -Os
          '';
        };
        apps = { };
      });
}
