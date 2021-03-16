let sources = import nix/sources.nix; in
# This module is supposed to be called with ‘nixpkgs.callPackage’
{ callPackage
, haskellPackages
, haskell

# Overridable dependencies
, __data-maybe-preserve ?
    let k = "data-maybe-preserve"; in
    haskellPackages.callCabal2nix k sources.${k} {}

# Build options
, __src ? (callPackage nix/clean-src.nix {}) ./. # A directory
, justStaticExecutable ? true
}:
let
  name = "xlib-keys-hack";
  pkg = extendedHaskellPackages.callCabal2nix name __src {};

  extendedHaskellPackages = haskellPackages.extend (self: super: {
    data-maybe-preserve = __data-maybe-preserve;
    ${name} = pkg;
  });

  justStaticExecutableFn =
    if justStaticExecutable
    then haskell.lib.justStaticExecutables
    else x: x;
in
justStaticExecutableFn pkg // {
  data-maybe-preserve = __data-maybe-preserve;
  haskellPackages = extendedHaskellPackages;
  haskellPackage = pkg;
}
