let sources = import ./sources.nix; in
{ pkgs ? import sources.nixpkgs {}
}:
let
  noCabalStuffFilter = fileName: fileType: ! (
    fileType == "directory" &&
    ! isNull (builtins.match "^dist(-newstyle)?$" (baseNameOf fileName))
  );

  filter = fileName: fileType:
    noCabalStuffFilter fileName fileType &&
    pkgs.lib.cleanSourceFilter fileName fileType;
in
  pkgs.nix-gitignore.gitignoreFilterRecursiveSource filter [ ../.gitignore ]
