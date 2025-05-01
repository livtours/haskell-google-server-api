{
  description = "google-server-api";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    # flake-utils.lib.eachDefaultSystem
    flake-utils.lib.eachSystem [ flake-utils.lib.system.x86_64-linux ] (system:
      let
        pkgs = import nixpkgs { inherit system; };

        _jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak
          (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.unmarkBroken pkg));

        haskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
          };
        };
      in rec {
        packages.google-server-api =
          pkgs.haskell.lib.justStaticExecutables
            (haskellPackages.callCabal2nix "google-server-api" ./. { });

        packages.default = packages.google-server-api;

        devShells.default = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
            fourmolu
            pkgs.rlwrap
          ];
          inputsFrom = [ self.packages.${system}.default.env ];
        };
      });
}
