{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    vere = {
      url = "github:urbit/vere?ref=master";
      flake = false;
    };
    murmur3 = {
      url = "github:PeterScott/murmur3";
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
  };
  outputs =
    inputs@{ self, nixpkgs, flake-utils, vere, murmur3, pdjson, sha256, ... }:
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
          softfloat =
            pkgs.callPackage ./softfloat.nix { inherit (inputs) softfloat; };
          nock = pkgs.runCommand "nock" { } ''
            cp -r ${vere}/pkg .
            chmod -R a+w pkg
            cp pkg/noun/platform/linux/rsignal.h pkg/noun/platform/rsignal.h
            patch -p1 <${./vere.patch}
            rm pkg/noun/*_tests.c pkg/noun/jets/e/aes* pkg/noun/jets/e/argon2.c \
              pkg/noun/jets/e/blake.c pkg/noun/jets/e/ed_* pkg/noun/jets/e/keccak.c \
              pkg/noun/jets/e/ripe.c pkg/noun/jets/e/scr.c pkg/noun/jets/e/secp.c \
              pkg/noun/jets/e/sha1.c pkg/noun/jets/e/shax.c 
            mkdir -p $out
            HOME=$TMPDIR
            ${pkgs.emscripten}/bin/emcc \
              pkg/noun/*.c pkg/noun/jets/*/*.c pkg/noun/jets/*.c pkg/c3/*.c pkg/ur/*.c \
              ${murmur3}/murmur3.c ${pdjson}/pdjson.c ${gmp}/lib/libgmp.a ${sha256}/sha256.c \
              ${softfloat}/lib/softfloat.a \
              -o $out/nock.js \
              -DU3_OS_linux \
              -DU3_OS_ENDIAN_little \
              -DU3_GUARD_PAGE \
              -sEXPORTED_RUNTIME_METHODS=cwrap \
              -Ipkg -Ipkg/noun -Ipkg/c3 -Ipkg/ur -Ipkg/ent -I. \
              -I${pdjson} -I${gmp}/include -I${murmur3} -I${pkgs.libsigsegv}/include \
              -I${softfloat}/include -I${sha256} \
              -sEXPORTED_FUNCTIONS=_u3n_nock_on \
              # -Os

          '';
        };
        apps = { };
      });
}
