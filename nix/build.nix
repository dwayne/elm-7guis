{ lib, runCommand

, buildElmApplication
}:

{ name
, noJekyll ? false
, elmOptions ? { enableDebugger = true; }
}:

let
  fs = lib.fileset;

  root = ../.;

  #
  # Generated using elm2nix lock elm.json review/elm.json
  #
  elmLock = ../elm.lock;

  js = buildElmApplication ({
    inherit elmLock;

    name = "${name}-js";

    src = fs.toSource {
      inherit root;
      fileset = fs.unions [
        ../review
        ../src
        ../tests
        ../elm.json
      ];
    };

    entry = [
      "src/Counter.elm"
      "src/TemperatureConverter.elm"
      "src/FlightBooker.elm"
      "src/Timer.elm"
      "src/Crud.elm"
      "src/CircleDrawer.elm"
      "src/Cells.elm"
    ];

    output = "app.js";
    outputMin = "app.js";

    doElmFormat = true;
    elmFormatSourceFiles = [ "review" "src" "tests" ];

    doElmTest = true;
    doElmReview = true;
  } // elmOptions);
in
runCommand name {
  inherit js;

  src = fs.toSource {
    inherit root;

    fileset = fs.unions [
      ../css
      ../html
      ../images
    ];
  };
} ''
  mkdir -p "$out/js"

  cp -r "$src"/html/*.html "$out"
  cp -r "$src/css" "$out"
  cp -r "$src/images" "$out"

  cp "$js/app.js" "$out/js/app.js"

  ${lib.optionalString noJekyll ''
    touch "$out/.nojekyll"
  ''}
''
