{ pkgs ? import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/d88e2d581fee6aa0daaea9551628684c5d5eb3cd.tar.gz";
  sha256 = "sha256:0fg5ndc9g7wqmfql0lkm4hi9fxfnq28vzknhr6xfsbwyh4haqlxd";
}) {} }:
# with pkgs.python311Packages;

pkgs.mkShell {
  buildInputs = [
    (pkgs.python311.withPackages (ps: with ps; [
      numpy
      pandas
      jwt
    ]))
    pkgs.google-cloud-sdk
  ];
}
