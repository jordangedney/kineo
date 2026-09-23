{
  description = "Kineo, a scrolling tiling window manager for macOS";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

      # Only what the build reads, so editing the README doesn't rebuild.
      src = nixpkgs.lib.fileset.toSource {
        root = ./.;
        fileset = nixpkgs.lib.fileset.unions [
          ./kineo.cabal
          ./LICENSE
          ./README.md
          ./app
          ./cbits
          ./config
          ./hyper
          ./src
          ./test
        ];
      };

      haskellPackages =
        pkgs:
        pkgs.haskell.packages.ghc912.override {
          overrides = final: _prev: { kineo = final.callCabal2nix "kineo" src { }; };
        };
    in
    {
      packages = forAllSystems (pkgs: rec {
        kineo = pkgs.haskell.lib.justStaticExecutables (haskellPackages pkgs).kineo;
        default = kineo;
      });

      # Both programs come from the one package.
      apps = forAllSystems (
        pkgs:
        let
          bin = name: {
            type = "app";
            program = "${self.packages.${pkgs.stdenv.hostPlatform.system}.kineo}/bin/${name}";
          };
        in
        rec {
          kineo = bin "kineo";
          kineo-hyper = bin "kineo-hyper";
          default = kineo;
        }
      );

      devShells = forAllSystems (
        pkgs:
        let
          hp = haskellPackages pkgs;
        in
        {
          default = hp.shellFor {
            packages = p: [ p.kineo ];
            nativeBuildInputs = [
              hp.cabal-install
              hp.haskell-language-server
              pkgs.haskellPackages.fourmolu
            ];
          };
        }
      );

      # Run Kineo as a launchd agent from nix-darwin:
      #   imports = [ kineo.darwinModules.default ];
      #   services.kineo.enable = true;
      darwinModules.default = import ./nix/darwin-module.nix self;

      formatter = forAllSystems (pkgs: pkgs.nixfmt);
    };
}
