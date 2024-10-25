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
          buildInputs = with pkgs; [
            (fenix.packages.${system}.combine
              (with fenix.packages.${system}.latest; [
                fenix.packages.${system}.targets.wasm32-unknown-unknown.latest.rust-std
                cargo
                rustc
              ]))
            wasm-pack
            wasm-tools
            cargo-flamegraph
          ];
        };
      });
}
