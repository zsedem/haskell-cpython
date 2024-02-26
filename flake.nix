{
  description = "Haskell CPython C bindings";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // ({
              cpython = prev.haskell.lib.overrideCabal (hfinal.callCabal2nix "cpython" ./. { }) (old: {
                buildTools = with final; [ autoconf automake ];
                preCompileBuildDriver = "autoreconf";
              });
            });
        };
      };
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let

          pkgs = import nixpkgs {
            inherit system;
            overlays = [ overlay ];
          };
        in
        {
          packages.default = pkgs.haskellPackages.cpython;

          devShells.default = pkgs.haskellPackages.shellFor {
            withHoogle = true;
            packages = p: with p; [ cpython ];
            buildInputs = (with pkgs; [
              cabal-install
              haskell-language-server
              hlint
              pkg-config
            ]);
            shellHook = pkgs.haskellPackages.cpython.preCompileBuildDriver;
          };
        }
      ) // {
      overlays.default = overlay;
    };

}
