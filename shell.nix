{ pkgs ? (import nix/default-nixpkgs-pick.nix).pkgs
}:
let
  xlib-keys-hack = import ./. { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = [ xlib-keys-hack ];
}
