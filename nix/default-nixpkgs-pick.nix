rec {
  src =
    let
      # ref "nixos-20.03", 22 August 2020
      commit = "0c59c1296b23abc25a6383ff26db2eeb17ad8a81";
    in
      fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
        sha256 = "03sifcpkc3prszaycd6snvpxam66phmj0b7m4723l5dmmsyq4bkw";
      };

  pkgs = import src {};
}
