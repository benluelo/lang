{
  description = "vm";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane = {
      url = "github:ipetkov/crane";
    };
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      treefmt-nix,
      rust-overlay,
      crane,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      flake = { };
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      imports = [ treefmt-nix.flakeModule ];

      perSystem =
        {
          config,
          self',
          pkgs,
          system,
          lib,
          ...
        }:
        let
          dbg =
            value:
            builtins.trace (
              if value ? type && value.type == "derivation" then
                "derivation ${value}"
              else
                pkgs.lib.generators.toPretty { } value
            ) value;
          pkgs = nixpkgs.legacyPackages.${system}.appendOverlays (
            with inputs; [ rust-overlay.overlays.default ]
          );

          rust = pkgs.rust-bin.fromRustupToolchain {
            channel = "nightly-2024-10-16";
            # this is the easiest way to pull in the least amount possible, even though rust-std
            # isn't required for all use cases (i.e. -Z build-std, where we use rust-src instead)
            #
            # it should be possible to construct the toolchains manually, but this works for now
            components = [
              "rustc"
              "cargo"
              "rustfmt"
              "rust-std"
              "rust-docs"
              "rust-analyzer"
              "clippy"
              "miri"
              "rust-src"
              "llvm-tools-preview"
            ];
          };

        in
        {
          _module = {
            args = {
              inherit
                nixpkgs
                pkgs
                dbg
                rust
                ;

            };
          };

          packages = { };

          devShells.default = pkgs.mkShell {
            name = "union-devShell";
            buildInputs =
              [ (dbg rust) ]
              ++ (with pkgs; [
                jq
                nil
              ]);
            nativeBuildInputs = [
              config.treefmt.build.wrapper
            ] ++ lib.attrsets.attrValues config.treefmt.build.programs;
          };
          # https//flake.parts/options/treefmt-nix#opt-perSystem.treefmt
          treefmt = {
            projectRootFile = "flake.nix";
            programs = {
              rustfmt = {
                enable = true;
                package = rust;
              };
              nixfmt = {
                enable = true;
                package = pkgs.nixfmt-rfc-style;
              };
            };
          };
        };
    };
}