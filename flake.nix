{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin"];

      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: let
        jdk = pkgs.jdk21_headless;
        sbt = pkgs.sbt.override {jre = jdk;};
        crossPkgs = pkgs.pkgsCross.riscv32-embedded;

        testBinaries = crossPkgs.stdenv.mkDerivation {
          name = "riscv-test-binaries";
          src = ./src/test/resources/cae;
          makeFlags = [
            "CC=${crossPkgs.stdenv.cc.targetPrefix}gcc"
            "OBJCOPY=${crossPkgs.stdenv.cc.targetPrefix}objcopy"
            "OBJDUMP=${crossPkgs.stdenv.cc.targetPrefix}objdump"
          ];
          installPhase = ''
            mkdir -p $out/bin
            cp bin/*.bin $out/bin/
          '';
        };
      in {
        packages = {
          test-bins = testBinaries;
        };

        devShells.user = pkgs.mkShell {
          packages = with pkgs; [
            sbt
            verilator
            python3
            jdk
            metals
          ];
          CHISEL_FIRTOOL_PATH = "${pkgs.circt}/bin";
          RISCV_TEST_BIN_DIR = "${testBinaries}/bin";
        };
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            sbt
            jdk
            verilator
            python3
          ];
          CHISEL_FIRTOOL_PATH = "${pkgs.circt}/bin";
          RISCV_TEST_BIN_DIR = "${testBinaries}/bin";
        };
      };
    };
}
