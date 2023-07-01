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

          packages = {
            cljnotes-clj = cljpkgs.mkCljBin {
              projectSrc = ./.;
              name = "org.suskalo/cljnotes";
              main-ns = "notes.core";
            };

            cljnotes-jdk = cljpkgs.customJdk {
              cljDrv = self.packages.${system}.cljnotes-clj;
              locales = "en";
            };

            cljnotes-graal = cljpkgs.mkGraalBin {
              cljDrv = self.packages."${system}".cljnotes-clj;
            };

            default = self.packages.${system}.cljnotes-clj;

          };

          apps.default = {
            type = "app";
            program = "${self.packages.${system}.default}/bin/cljnotes";
          };
        }
      );
}
