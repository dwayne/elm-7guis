{
  inputs = {
    deploy = {
      url = "github:dwayne/deploy";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    elm2nix = {
      url = "github:dwayne/elm2nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, deploy, elm2nix }:
    flake-utils.lib.eachDefaultSystem(system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (elm2nix.lib.elm2nix pkgs) buildElmApplication;

        workshop = pkgs.callPackage ./nix/workshop.nix {};

        build = pkgs.callPackage ./nix/build.nix { inherit buildElmApplication; };

        dev = build { name = "elm-7guis-dev"; };

        prod = build {
          name = "elm-7guis-prod";
          elmOptions = {
            enableOptimizations = true;
            optimizeLevel = 1;

            doMinification = true;
            useTerser = true;
          };
        };

        serve = pkgs.callPackage ./nix/serve.nix {};

        serveWorkshop = serve {
          name = "serve-elm-7guis-workshop";
          root = workshop;
          port = 8001;
        };

        serveDev = serve {
          name = "serve-elm-7guis-dev";
          root = dev;
        };

        serveProd = serve {
          name = "serve-elm-7guis-prod";
          root = prod;
        };

        deployProd = pkgs.writeShellScript "deploy-elm-7guis-prod" ''
          ${deploy.packages.${system}.default}/bin/deploy "$@" ${prod} gh-pages
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
            deploy.packages.${system}.default
            elm2nix.packages.${system}.default
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.elmPackages.elm-json
            pkgs.elmPackages.elm-review
            pkgs.elmPackages.elm-test
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

            build () {
              nix build "''${@:--L}"
            }
            alias b='build'

            serve () {
              nix run "$@"
            }
            alias s='serve'

            build-prod () {
              nix build .#prod "''${@:--L}"
            }
            alias bp='build'

            serve-prod () {
              nix run .#prod "$@"
            }
            alias sp='serve'

            clean () {
              rm -rf "$PROJECT_ROOT"/{elm-stuff,result}
            }
            alias c='clean'

            f () {
              elm-format "$PROJECT_ROOT/src" "''${@:---yes}"
            }

            r () {
              elm-review "$@"
            }

            t () {
              elm-test "$@"
            }

            d () {
              nix run .#deploy "$@"
            }

            echo "Development environment loaded"
            echo ""
            echo "Type 'bw' to build the workshop"
            echo "Type 'sw' to serve the workshop"
            echo ""
            echo "Type 'b' to build the development version of the application"
            echo "Type 's' to serve the development version of the application"
            echo ""
            echo "Type 'bp' to build the production version of the application"
            echo "Type 'sp' to serve the production version of the application"
            echo ""
            echo "Type 'c' to remove build artifacts"
            echo "Type 'f' to run elm-format"
            echo "Type 'r' to run elm-review"
            echo "Type 't' to run elm-test"
            echo ""
            echo "Type 'd' to deploy the production version of the application to GitHub Pages"
          '';
        };

        packages = {
          inherit workshop dev prod;
          default = dev;
        };

        apps = {
          default = self.apps.${system}.dev;

          workshop = mkApp {
            drv = serveWorkshop;
            description = "Serve the 7GUIs workshop";
          };

          dev = mkApp {
            drv = serveDev;
            description = "Serve the development version of the 7GUIs web application";
          };

          prod = mkApp {
            drv = serveProd;
            description = "Serve the production version of the 7GUIs web application";
          };

          deploy = mkApp {
            drv = deployProd;
            description = "Deploy the production version of the 7GUIs web application";
          };
        };

        checks = {
          inherit workshop dev prod serveDev serveProd deployProd;
        };
      }
    );
}
