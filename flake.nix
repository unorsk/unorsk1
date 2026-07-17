{
  # Hakyll blog for unorsk.com.
  #
  # Usage:
  #
  #   nix develop                 # dev shell with ghc, cabal, and haskell-language-server
  #     cabal run site -- watch   #   ...then serve the site locally at http://127.0.0.1:8000
  #     cabal run site -- build   #   ...or just build it into docs/
  #
  #   nix run . -- watch          # same as above, but without entering a shell
  #   nix run . -- rebuild        # full clean rebuild of the site into docs/
  #
  #   nix build                   # build the `site` generator executable into ./result/bin/site
  #
  # Note: flakes only see files tracked in git. This repo is jj-colocated,
  # so if nix complains about a missing file, run `jj st` once to snapshot
  # the working copy and sync the git index.
  description = "unorsk.com — Hakyll static site generator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "x86_64-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system:
        f nixpkgs.legacyPackages.${system});
    in
    {
      packages = forAllSystems (pkgs: rec {
        # Written by hand (mirroring unorsk1.cabal) instead of callCabal2nix,
        # because cabal2nix needs import-from-derivation, which is disabled
        # in this nix setup.
        site = pkgs.haskellPackages.mkDerivation {
          pname = "unorsk1";
          version = "0.1.0.0";
          src = ./.;
          isLibrary = false;
          isExecutable = true;
          executableHaskellDepends = with pkgs.haskellPackages; [ base hakyll ];
          license = pkgs.lib.licenses.mit;
          mainProgram = "site";
        };
        default = site;
      });

      devShells = forAllSystems (pkgs: {
        default = pkgs.haskellPackages.shellFor {
          packages = _: [ self.packages.${pkgs.stdenv.hostPlatform.system}.site ];
          nativeBuildInputs = with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
          ];
        };
      });
    };
}
