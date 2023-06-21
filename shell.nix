let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/c4434c7ac529d3e69617766daaaca101fe8cecc0.tar.gz") {};
in
pkgs.mkShell {
  packages = [
    pkgs.caddy
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-review
    pkgs.elmPackages.elm-test
  ];

  shellHook =
    ''
    export project="$PWD"
    export build="$project/.build"

    export PATH="$project/bin:$PATH"
    '';
}
