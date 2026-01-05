{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
    ...
  }: let
    systems = ["x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin"];
    forAllSystems = nixpkgs.lib.genAttrs systems;
  in {
    overlays.default = final: prev: let
      jdk = prev.jdk25_headless;
    in {
      sbt = prev.sbt.override {jre = jdk;};
    };

    devShells = forAllSystems (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [self.overlays.default];
        };
      in {
        default = pkgs.mkShell {
          packages = with pkgs; [sbt verilator circt python3 coursier jdk25_headless];
          CHISEL_FIRTOOL_PATH = "${pkgs.circt}/bin";
        };
      }
    );
  };
}
