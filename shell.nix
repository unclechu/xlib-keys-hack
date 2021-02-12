let sources = import nix/sources.nix; in
args@
{ pkgs ? import sources.nixpkgs {}
, data-maybe-preserve ? null
, src ? null
}:
let
  xlib-keys-hack = import ./. args;
in
pkgs.mkShell {
  buildInputs = [
    xlib-keys-hack
  ];
}
