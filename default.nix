let sources = import nix/sources.nix; in
{ pkgs ? import sources.nixpkgs {}

, data-maybe-preserve ?
    let k = "data-maybe-preserve"; in
    pkgs.haskellPackages.callCabal2nix k sources.${k} {}

, src ? (import nix/clean-src.nix { inherit pkgs; }) ./. # A directory
, justStaticExecutable ? true
}:
let
  name = "xlib-keys-hack";
  pkg = haskellPackages.callCabal2nix name src {};

  haskellPackages = pkgs.haskellPackages.extend (self: super: {
    inherit data-maybe-preserve;
    ${name} = pkg;
  });

  justStaticExecutableFn =
    if justStaticExecutable
    then pkgs.haskell.lib.justStaticExecutables
    else x: x;
in
justStaticExecutableFn pkg // {
  inherit src data-maybe-preserve;
  inherit haskellPackages;
  haskellPackage = pkg;
}
