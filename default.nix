let sources = import nix/sources.nix; in
{ pkgs ? import sources.nixpkgs {}

, data-maybe-preserve ?
    let k = "data-maybe-preserve"; in
    pkgs.haskellPackages.callCabal2nix k sources.${k} {}

, src ? ./. # A directory
, packageName ? "xlib-keys-hack"
, justStaticExecutable ? true
}:
let
  haskellPackages = pkgs.haskellPackages.extend (self: super: {
    inherit data-maybe-preserve;
    ${packageName} = xlib-keys-hack;
  });

  cleanSrcDir = dir:
    let
      noCabalStuffFilter = fileName: fileType: ! (
        fileType == "directory" &&
        ! isNull (builtins.match "^dist(-newstyle)?$" (baseNameOf fileName))
      );
    in
      pkgs.lib.cleanSourceWith {
        name   = "${packageName}-clean-source";
        filter = noCabalStuffFilter;
        src    = pkgs.lib.cleanSource dir;
      };

  justStaticExecutableFn =
    if justStaticExecutable
    then pkgs.haskell.lib.justStaticExecutables
    else x: x;

  cleanSrc = cleanSrcDir src;
  xlib-keys-hack = haskellPackages.callCabal2nix packageName cleanSrc {};
in
justStaticExecutableFn xlib-keys-hack // {
  inherit src cleanSrc data-maybe-preserve;
  inherit haskellPackages;
  haskellPackage = xlib-keys-hack;
}
