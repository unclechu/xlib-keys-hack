rec {
  src =
    let
      # ref "nixos-20.09", 29 September 2020
      commit = "0cfe5377e8993052f9b0dd56d058f8008af45bd9";
    in
      fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
        sha256 = "0i3ybddi2mrlaz3di3svdpgy93zwmdglpywih4s9rd3wj865gzn1";
      };

  pkgs = import src {};
}
