{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "flake-utils";
  };

  outputs = inputs@{ self, flake-utils, nixpkgs, ... }:
    let supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import ./nix/overlay.nix) ];
        };
        parsedSystem = pkgs.lib.systems.parse.mkSystemFromString system;
        fenix = inputs.fenix.packages.${system};
      in {
        devShells.default = pkgs.mkShell {
          LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
          buildInputs = [
            (fenix.combine [
              (fenix.complete.withComponents [
                "cargo"
                "clippy"
                "rustc"
                "rustfmt"
                "rust-src"
              ])
              fenix.targets.wasm32-unknown-unknown.latest.rust-std
            ])
            pkgs.bacon
            pkgs.iconv
            pkgs.pkg-config
            pkgs.urcrypt
            pkgs.wasm-pack
          ] ++ (nixpkgs.lib.lists.optional
            (parsedSystem.kernel.name != "darwin")
            pkgs.gdb) # nixpkgs won't build gdb for darwin
            ++ (nixpkgs.lib.lists.optional (parsedSystem.kernel.name != "darwin"
              || parsedSystem.cpu.name != "x86_64")
              pkgs.cargo-watch); # nixpkgs won't build cargo-watch for darwin-x86
        };
      });
}
