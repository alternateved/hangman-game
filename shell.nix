let
  pkgs = import ./packages.nix { };
  package = import ./.;

  haskellTools = with pkgs; [
    ghc
    ghcid
    cabal-install
    haskellPackages.hindent
    haskellPackages.hoogle
    haskellPackages.ormolu
    haskellPackages.haskell-language-server
  ];

  haskellDeps = with pkgs; [ haskellPackages.split haskellPackages.random ];

in pkgs.mkShell {
  nativeBuildInputs = [ pkgs.pkg-config ];
  inputsFrom = [ package.env ];
  buildInputs = haskellTools ++ haskellDeps;
  shellHook = ''
    export PS1="\[hs:\033[1;32m\]\W\[\033[0m\] ~ "
  '';
}
