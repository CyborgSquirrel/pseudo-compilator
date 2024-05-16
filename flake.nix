{
  description = "A very basic flake";
  
  inputs = {
    # nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
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
        devShell = pkgs.mkShell rec {
          buildInputs = [
            pkgs.rustc
            # (pkgs.rust-bin.selectLatestNightlyWith (toolchain: toolchain.default))
            pkgs.rust-analyzer

            pkgs.cargo

            pkgs.llvmPackages_17.libllvm
            pkgs.llvmPackages_17.lld
            pkgs.libffi
            pkgs.libxml2

            pkgs.llvmPackages_17.libcxxClang

            pkgs.gdb

            (pkgs.python311.withPackages
              (pythonPkgs: [
                pythonPkgs.starlette
                pythonPkgs.pydantic
                pythonPkgs.uvicorn
                pythonPkgs.websockets

                pythonPkgs.polars
              ])
            )
          ];
          LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath buildInputs}";
        };
      }
    );
}
