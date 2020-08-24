let
  defaultPkgs = (import nix/default-nixpkgs-pick.nix).pkgs;

  default-data-maybe-preserve = pkgs:
    let
      src = pkgs.fetchFromGitHub {
        owner = "unclechu";
        repo = "haskell-data-maybe-preserve";
        # ref "master", 21 April 2018
        rev = "705e3e4e85661c6c3c34f43a408b111433abb27e";
        sha256 = "1m6vgkwp1c7n8yxfpsh6vgkvi8lj1svcq99js70gq54m7z9lffsb";
      };
    in
      pkgs.haskellPackages.callCabal2nix "data-maybe-preserve" src {};
in
args@
{ pkgs                ? defaultPkgs
, data-maybe-preserve ? default-data-maybe-preserve (args.pkgs or defaultPkgs)
, src                 ? ./.
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
