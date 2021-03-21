# Author: Viacheslav Lotsmanov
# License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE
let sources = import nix/sources.nix; in
args@
{ pkgs ? import sources.nixpkgs {}

# Forwarded overridable dependencies
, __data-maybe-preserve ? null

# Forwarded build options
, __src ? null
, justStaticExecutable ? false # N.B. Default value is different here

# Overridable local dependencies
, nix-utils ? pkgs.callPackage sources.nix-utils {}

# Local options
, withCabal ? false
, withHpack ? false
, withStack ? false
, withPackageRepl ? false # Adds package library modules into GHCi REPL
, withHoogle ? true
, buildExecutable ? true
}:
let
  forwardedNames = [ "__data-maybe-preserve" "__src" ];
  filterForwarded = pkgs.lib.filterAttrs (n: v: builtins.elem n forwardedNames);
  forwardedArgs = { inherit justStaticExecutable; } // filterForwarded args;
  pkg = pkgs.callPackage ./. forwardedArgs;
  hp = pkg.haskellPackages;
  name = pkg.haskellPackage.pname;

  inherit (nix-utils) wrapExecutable;
  pkgReplGhc = hp.ghcWithPackages (p: [p.${name}]);

  # Produces ‘PACKAGE-NAME-ghc’ and ‘PACKAGE-NAME-ghci’ files.
  # ‘shellFor’ overrides ‘ghc’ and ‘ghci’ executables.
  pkgRepl =
    let
      exe = binName:
        wrapExecutable
          "${pkgReplGhc}/bin/${binName}"
          { name = "${name}-${binName}"; };
    in [
      (exe "ghci")
      (exe "ghc")
    ];

  hpack = pkgs.haskell.lib.justStaticExecutables hp.hpack;
  cabal = pkgs.haskell.lib.justStaticExecutables hp.cabal-install;
  stack = pkgs.haskell.lib.justStaticExecutables hp.stack;
in
hp.shellFor {
  packages = p: [
    p.${name}
  ];

  inherit withHoogle;

  buildInputs =
    (if withCabal then [ cabal ] else []) ++
    (if withHpack then [ hpack ] else []) ++
    (if withStack then [ stack ] else []) ++
    (if buildExecutable then [ hp.${name} ] else []) ++
    (if withPackageRepl then pkgRepl else []);
}
