# Author: Viacheslav Lotsmanov
# License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

let sources = import nix/sources.nix; in

# This module is supposed to be called with ‘nixpkgs.callPackage’
{ pkgs ? import sources.nixpkgs {}

, lib ? pkgs.lib
, symlinkJoin ? pkgs.symlinkJoin
, makeBinaryWrapper ? pkgs.makeBinaryWrapper

, haskellPackages ? pkgs.haskellPackages
, haskell ? pkgs.haskell
, haskell-language-server ? pkgs.haskell-language-server
, stack ? pkgs.stack

, xinput ? pkgs.xinput
, libX11 ? pkgs.libX11
, libXtst ? pkgs.libXtst

# Overridable dependencies
, __data-maybe-preserve ?
    let k = "data-maybe-preserve"; in
    haskellPackages.callCabal2nix k sources.${k} {}

# nix-shell options
, inNixShell ? false
, withHoogle ? true
, withStack ? false

# Build options
, __src ? ./.
}:

let
  pkgs = null; # Prevent direct access
  name = "xlib-keys-hack";

  src =
    let
      extensionOf = path:
        let match = builtins.match ".*\\.([^./]+)$" (builtins.baseNameOf path);
        in if match == null then null else builtins.elemAt match 0;
    in
      builtins.path {
        name = "${name}-source";
        path = __src;

        # Note that `cabal.project` does not end up here (it’s not neccesary for
        # the build). `cabal.project` disables optimizations for HLS and
        # development `cabal build`/`cabal run`/`cabal repl`. But since it’s not
        # added here for the Nix build of the executable default optimization
        # level is applied. So `cabal.project` is intentionally avoided here.
        filter = path: type:
          (
            type == "directory" &&
            builtins.match "^(app|src|test|[A-Z][A-Za-z0-9]+)$" (baseNameOf path) != null
          ) || (
            type == "regular" &&
            builtins.elem (extensionOf path) ["hs" "hsc" "cabal"]
          ) || (
            type == "regular" &&
            builtins.elem (baseNameOf path) ["LICENSE"]
          );
      };

  hsPkgs = haskellPackages.extend (self: super: {
    ${name} = hsPkgs.callCabal2nix name src {};
    data-maybe-preserve = __data-maybe-preserve;
    linux-evdev =
      haskell.lib.compose.overrideCabal
        (old: { broken = false; })
        (haskell.lib.doJailbreak super.linux-evdev);
  });

  runtimeDependencies = [
    xinput
  ];

  xlib-keys-hack =
    let pkg = haskell.lib.justStaticExecutables hsPkgs.${name}; in
    symlinkJoin (lib.fix (x: {
      pname = "${name}-wrapped";
      name = x.pname;
      meta.mainProgram = name;
      nativeBuildInputs = [ makeBinaryWrapper ];
      paths = [ pkg ];
      postBuild = ''
        wrapProgram "$out"/bin/${lib.escapeShellArg x.meta.mainProgram} \
          --prefix PATH : ${lib.escapeShellArg (lib.makeBinPath (
            runtimeDependencies
            # For `xlib-keys-hack-watch-for-window-focus-events` executable
            ++ [ pkg ]
          ))}
      '';
    }));

  # LSP for Haskell
  hls = haskell-language-server.override {
    haskellPackages = hsPkgs;
    supportedGhcVersions = [
      (builtins.replaceStrings ["."] [""] hsPkgs.ghc.version)
    ];
  };

  shell = hsPkgs.shellFor {
    packages = p: [
      p.${name}
    ];

    inherit withHoogle;

    shellHook = ''
      # Needed for `cabal repl` and HLS (Haskell Language Server LSP) to work.
      export LD_LIBRARY_PATH=${
        lib.escapeShellArg (lib.makeLibraryPath [libX11 libXtst])
      }"''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH"
    '';

    buildInputs =
      [
        hsPkgs.cabal-install
        hsPkgs.hlint
        hsPkgs.hpack
        hls
      ]
      ++ lib.optional withStack stack
      ++ runtimeDependencies;
  };
in

(if inNixShell then shell else xlib-keys-hack) // {
  inherit shell xlib-keys-hack;
  data-maybe-preserve = __data-maybe-preserve;
  haskellPackages = hsPkgs;
  haskellPackage = hsPkgs.${name};
  haskell-language-server = hls;
}
