let sources = import nix/sources.nix; in
# Forwarded arguments
args@
{ pkgs ? import sources.nixpkgs {}
, data-maybe-preserve ? null
, src ? null
, packageName ? null
, justStaticExecutable ? false

# Local arguments
, utils ? import sources.nix-utils { inherit pkgs; }
, withCabal ? false
, withStack ? false
, withPackageRepl ? false # Adds ‘xlib-keys-hack’ library modules into GHCi REPL
, withHoogle ? true
, buildExecutable ? true
}:
let
  forwardedNames = [ "pkgs" "data-maybe-preserve" "src" "packageName" ];
  filterForwarded = pkgs.lib.filterAttrs (n: v: builtins.elem n forwardedNames);
  forwardedArgs = { inherit justStaticExecutable; } // filterForwarded args;
  xlib-keys-hack = import ./. forwardedArgs;
  hp = xlib-keys-hack.haskellPackages;
  name = xlib-keys-hack.haskellPackage.pname;

  inherit (utils) wrapExecutable;
  pkgReplGhc = hp.ghcWithPackages (p: [p.xlib-keys-hack]);

  # Produces ‘xlib-keys-hack-ghc’ and ‘xlib-keys-hack-ghci’ files.
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
in
hp.shellFor {
  packages = p: [
    p.${name}
  ];

  inherit withHoogle;

  buildInputs =
    (if withCabal then [ hp.cabal-install ] else []) ++
    (if withStack then [ hp.stack ] else []) ++
    (if buildExecutable then [ hp.${name} ] else []) ++
    (if withPackageRepl then pkgRepl else []);
}
