{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.mkDerivation {
  pname = "uniqueness-periods-vector-stats";
  version = "0.4.0.0";
  src = ./.;
  libraryHaskellDepends = [ pkgs.haskellPackages.base pkgs.haskellPackages.ghc-prim ];
  homepage = "https://hackage.haskell.org/package/uniqueness-periods-vector-stats";
  description = "A very basic descriptive statistics";
  license = pkgs.lib.licenses.mit;
}
