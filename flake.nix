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

        # Kineo.app: the window manager and the hyper key in one process,
        # with a menu bar icon. Signed ad hoc; `nix run .#install` signs it
        # with your certificate.
        app = pkgs.runCommand "kineo-app-${kineo.version}" { } ''
          contents=$out/Applications/Kineo.app/Contents
          mkdir -p "$contents/MacOS"
          cp ${kineo}/bin/kineo "$contents/MacOS/kineo"
          substitute ${./nix/Info.plist} "$contents/Info.plist" --subst-var-by version ${kineo.version}
        '';
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

          # Build Kineo.app, sign it, put it in ~/Applications and start it.
          # macOS ties Accessibility access to the signature, so it must be
          # the same certificate every time: $KINEO_SIGN_IDENTITY, or the
          # first Developer ID or Apple Development one in the keychain.
          install = {
            type = "app";
            program = "${
              pkgs.writeShellApplication {
                name = "kineo-install";
                text = ''
                  pkg=${self.packages.${pkgs.stdenv.hostPlatform.system}.app}
                  kineo=${self.packages.${pkgs.stdenv.hostPlatform.system}.kineo}/bin/kineo
                  dest="$HOME/Applications/Kineo.app"
                  identity="''${KINEO_SIGN_IDENTITY:-$(/usr/bin/security find-identity -v -p codesigning \
                    | grep -Eo '"(Developer ID Application|Apple Development): [^"]+"' | head -n 1 | tr -d '"' || true)}"
                  if [ -z "$identity" ]; then
                    echo "kineo-install: no code signing certificate found; set KINEO_SIGN_IDENTITY" >&2
                    exit 1
                  fi

                  # Only one Kineo runs at a time.
                  if "$kineo" send quit >/dev/null 2>&1; then
                    echo "stopping the running Kineo"
                    for _ in $(seq 50); do
                      "$kineo" send quit >/dev/null 2>&1 || break
                      sleep 0.1
                    done
                  fi

                  mkdir -p "$HOME/Applications"
                  rm -rf "$dest"
                  cp -R "$pkg/Applications/Kineo.app" "$dest"
                  chmod -R u+w "$dest"
                  /usr/bin/codesign --force --sign "$identity" "$dest"
                  echo "installed $dest, signed by $identity"
                  /usr/bin/open "$dest"
                '';
              }
            }/bin/kineo-install";
          };

          # For hacking: both programs from one terminal, debug logs on.
          # Arguments go to kineo. Quitting kineo also stops kineo-hyper,
          # which puts Caps Lock back.
          dev = {
            type = "app";
            program = "${
              pkgs.writeShellApplication {
                name = "kineo-dev";
                text = ''
                  bin=${self.packages.${pkgs.stdenv.hostPlatform.system}.kineo}/bin
                  "$bin/kineo-hyper" --escape &
                  hyper=$!
                  trap 'kill "$hyper" 2>/dev/null || true' EXIT INT TERM
                  KINEO_LOG="''${KINEO_LOG:-debug}" "$bin/kineo" "$@"
                '';
              }
            }/bin/kineo-dev";
          };
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
