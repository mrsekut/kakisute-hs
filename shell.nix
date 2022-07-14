{
  compiler ? "ghc901",
}:

let
  pkgs = import <nixpkgs> { };
  ghc = pkgs.haskell.packages.${compiler};
  cabal = ghc.cabal-install;
  ghcWithPackages = ghc.ghcWithPackages (hpkgs: with hpkgs; [ ]);

in pkgs.mkShell {
  buildInputs = with pkgs; [
    cabal
    ghcWithPackages
    stack
    hlint
    haskell-language-server
  ];
}