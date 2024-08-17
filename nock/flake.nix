{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "flake-utils";
  };

  outputs = { self, flake-utils, nixpkgs }:
    let supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        devShells.default = pkgs.mkShell {
          # CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER =
          #   "lld"; # LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
          buildInputs = with pkgs; [
            wasm-pack
            rustc
            wasm-tools
            cargo
            llvmPackages.bintools
          ];
        };
      });
}
