{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "flake-utils";
    fenix.url = "github:nix-community/fenix";
  };

  outputs = { self, flake-utils, nixpkgs, fenix }:
    let supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        inherit fenix;
        devShells.default = pkgs.mkShell {
          # CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER =
          #   "lld"; # LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
          buildInputs = with pkgs;
            with fenix.packages.${system}.latest; [
              wasm-pack
              rustc
              wasm-tools
              cargo
              llvmPackages.bintools
            ];
        };
      });
}
