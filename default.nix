# Author: Viacheslav Lotsmanov
# License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE
let sources = import nix/sources.nix; in
# This module is supposed to be called with ‘nixpkgs.callPackage’
{ lib
, callPackage
, fetchFromGitHub
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
    linux-evdev =
      haskell.lib.compose.overrideCabal
        (old: { broken = false; })
        (haskell.lib.doJailbreak super.linux-evdev);

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
