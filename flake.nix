{
  description = "my project description";
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/haskell-updates;
    flake-utils.url = "github:numtide/flake-utils";
    hls-flake.url = "git+https://github.com/haskell/haskell-language-server";
    bsb-http-chunked = {
      url = "github:sjakobi/bsb-http-chunked";
      flake = false;
    };
    weigh = {
      url = "github:ysangkok/weigh/janus/import-unless";
      flake = false;
    };
  };

  outputs = inputs@{
    self,
    nixpkgs,
    flake-utils,
    hls-flake,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        config = {allowUnfree = true;};
      };

      hls = hls-flake.packages."${system}".haskell-language-server-92;

      hPkgs = pkgs.haskell.packages.ghc92;

      ghc-wrapped = hPkgs.ghcWithHoogle (hPkgs: with hPkgs; [
        megaparsec
        parser-combinators
        containers
        transformers
        lens
        HUnit
        semigroupoids
        adjunctions
        record-hasfield
      ]);

      stack-wrapped = pkgs.symlinkJoin {
        name = "stack"; # will be available as the usual `stack` in terminal
        paths = [ hPkgs.stack ];
        buildInputs = [pkgs.makeWrapper];
        postBuild = ''
          wrapProgram $out/bin/stack \
            --add-flags "\
              --no-nix \
              --system-ghc \
              --no-install-ghc \
            "
        '';
      };

      haskellDevEnv = [ ghc-wrapped stack-wrapped ] ++ (with hPkgs; [ cabal-install ]) ++ [ hls ];

    in {
      devShells.default = pkgs.mkShell {
        buildInputs = [pkgs.zlib];

        nativeBuildInputs = with hPkgs; [
          ghc-wrapped
          stack-wrapped
          cabal-install
          fourmolu
          implicit-hie
          hpack
          hlint
          hls
          (pkgs.vscode-with-extensions.override {
            vscodeExtensions = with pkgs.vscode-extensions;
              [
                haskell.haskell
                justusadam.language-haskell
                mkhl.direnv
              ];
          })
        ];

        withHoogle = true;
      };
    });
}
