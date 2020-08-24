args@
{ pkgs ? import <nixpkgs> {}
}:
let
  xlib-keys-hack = import ./. args;
in
pkgs.mkShell {
  buildInputs = [ xlib-keys-hack ];
}
