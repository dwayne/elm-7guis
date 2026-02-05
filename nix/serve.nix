{ caddy, writeShellScript }:

{ name # The name of the script
, root # The derivation that contains the files to be served
, port ? 8000
}:

writeShellScript name ''
  "${caddy}/bin/caddy" file-server --browse --root "${root}" --listen :${builtins.toString(port)}
''
