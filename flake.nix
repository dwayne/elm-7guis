{
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem(system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        workshop = pkgs.callPackage ./nix/workshop.nix {};
        serveWorkshop = pkgs.writeShellScript "serve-elm-7guis-workshop" ''
          "${pkgs.caddy}/bin/caddy" file-server --browse --root "${workshop}" --listen :8001
        '';

        mkApp = { drv, description }: {
          type = "app";
          program = "${drv}";
          meta.description = description;
        };
      in
      {
        devShells.default = pkgs.mkShell {
          name = "elm-7guis";

          packages = [
            pkgs.caddy
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.elmPackages.elm-json
            pkgs.elmPackages.elm-review
            pkgs.elmPackages.elm-test
            pkgs.nodejs_24
          ];

          shellHook = ''
            export PROJECT_ROOT="$PWD"
            export PS1="($name)\n$PS1"

            build-workshop () {
              nix build .#workshop "''${@:--L}"
            }
            alias bw='build-workshop'

            serve-workshop () {
              nix run .#workshop "$@"
            }
            alias sw='serve-workshop'

            f () {
              elm-format "$PROJECT_ROOT/src" "''${@:---yes}"
            }

            r () {
              elm-review "$@"
            }

            t () {
              elm-test "$@"
            }

            echo "Development environment loaded"
            echo ""
            echo "Type 'bw' to build the workshop"
            echo "Type 'sw' to serve the workshop"
            echo ""
            echo "Type 'f' to run elm-format"
            echo "Type 'r' to run elm-review"
            echo "Type 't' to run elm-test"
            echo ""
          '';
        };

        packages = {
          inherit workshop;
        };

        apps = {
          workshop = mkApp {
            drv = serveWorkshop;
            description = "Serve the 7GUIs workshop";
          };
        };
      }
    );
}
