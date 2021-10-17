{
  compiler ? "ghc901",
}:

let
  pkgs = import <nixpkgs> { };
  cabal = pkgs.haskellPackages.cabal-install;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (hpkgs: with hpkgs; [
  ]);

in pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    cabal
    ghc
    stack
    hlint
  ];
}