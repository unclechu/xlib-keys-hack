let sources = import nix/sources.nix; in
{ pkgs ? import sources.nixpkgs {}

, data-maybe-preserve ?
    let k = "data-maybe-preserve"; in
    pkgs.haskellPackages.callCabal2nix k sources.${k} {}

, src ? ./.
}:
let
  hs = pkgs.haskellPackages.extend (self: super: {
    inherit data-maybe-preserve;
  });

  package = hs.callCabal2nix "xlib-keys-hack" src {};
in
pkgs.haskell.lib.justStaticExecutables package // {
  inherit src data-maybe-preserve;
  haskellPackages = hs;
  haskellPackage = package;
}
