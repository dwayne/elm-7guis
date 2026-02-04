{ lib, runCommand }:

let
  fs = lib.fileset;
in
runCommand "elm-7guis-workshop" {
  src = fs.toSource {
    root = ../.;
    fileset = fs.unions [
      ../css
      ../images
      ../workshop
    ];
  };
} ''
  mkdir "$out"

  cp -r "$src/css" "$out"
  cp -r "$src/images" "$out"
  cp -r "$src/workshop"/* "$out"
''
