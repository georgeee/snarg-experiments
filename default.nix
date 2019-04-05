with import <nixpkgs> {};

stdenv.mkDerivation {
  src = "./.";
  name = "snarg-playground";

  shellHook = ''
    ghci 
  '';

  buildInputs = [
    (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
      arithmoi
      random
      MonadRandom
      (haskell.lib.dontCheck ( universum ) )
    ]) )
  ];
}
