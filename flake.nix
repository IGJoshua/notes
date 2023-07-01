{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    clj-nix = {
      url = "github:jlesquembre/clj-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, flake-utils, clj-nix }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          cljpkgs = clj-nix.packages.${system};
        in
        {
          devShells.default = import ./shell.nix { inherit pkgs; };
          packages = rec {
            cljnotes-clj = cljpkgs.mkCljBin {
              projectSrc = ./.;
              name = "org.suskalo/cljnotes";
              main-ns = "notes.core";
              jdkRunner = pkgs.jdk17_headless;
              buildCommand = "clj -T:build uber";
            };
            cljnotes-jdk = cljpkgs.customJdk {
              cljDrv = self.packages.${system}.cljnotes-clj;
              locales = "en";
            };
            default = cljnotes-jdk;
          };
          apps.default = {
            type = "app";
            program = "${self.packages.${system}.default}/bin/cljnotes";
          };
        }
      );
}
