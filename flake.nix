{
  description = "A very basic flake";
  
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };
  
  outputs = {
    self,
    nixpkgs,
    flake-utils, rust-overlay,
  }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            rust-overlay.overlays.default
          ];
        };
      in {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.rustc
            # (pkgs.rust-bin.selectLatestNightlyWith (toolchain: toolchain.default))
            pkgs.rust-analyzer

            pkgs.cargo
          ];
          # LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath buildInputs}";
        };
      }
    );
}
