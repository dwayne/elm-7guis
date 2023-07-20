let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/7f256d7da238cb627ef189d56ed590739f42f13b.tar.gz") {};
in
pkgs.mkShell {
  packages = [
    pkgs.caddy
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-optimize-level-2
    pkgs.elmPackages.elm-review
    pkgs.elmPackages.elm-test
    pkgs.nodejs_18
    pkgs.shellcheck
  ];

  shellHook =
    ''
    export project="$PWD"
    export build="$project/.build"

    export PATH="$project/bin:$PATH"
    '';
}
