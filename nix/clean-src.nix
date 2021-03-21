# Author: Viacheslav Lotsmanov
# License: GPLv3 https://raw.githubusercontent.com/unclechu/xlib-keys-hack/master/LICENSE

# This module is supposed to be called with ‘nixpkgs.callPackage’
{ lib, nix-gitignore }:
let
  withoutDeadWeight = fileName: fileType: ! (
    builtins.elem (baseNameOf fileName) [
      ".editorconfig"
      ".travis.yml"
      "Makefile"
      "stack.yaml.lock"
    ]
    ||
    ! isNull (builtins.match "^.*\.(md|yaml|yml)$" fileName)
    ||
    (
      fileType == "directory" &&
      ! isNull (builtins.match "^.*/docs/scheme$" fileName)
    )
  );

  noCabalStuffFilter = fileName: fileType: ! (
    fileType == "directory" &&
    ! isNull (builtins.match "^dist(-newstyle)?$" (baseNameOf fileName))
  );

  filter = fileName: fileType:
    noCabalStuffFilter    fileName fileType &&
    withoutDeadWeight     fileName fileType &&
    lib.cleanSourceFilter fileName fileType;
in
  nix-gitignore.gitignoreFilterRecursiveSource filter [ ../.gitignore ]
