{
  description = "my project description";
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs@{ self
    , nixpkgs
    , flake-utils
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
      };

      hls = pkgs.haskell-language-server;

      hPkgs = pkgs.haskellPackages;

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
        filepath
        mtl-compat
        ieee754
      ]);

      stack-wrapped = with pkgs; symlinkJoin {
        name = "stack"; # will be available as the usual `stack` in terminal
        paths = [ stack ];
        buildInputs = [ makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/stack \
            --add-flags "\
              --no-nix \
              --system-ghc \
              --no-install-ghc \
            "
        '';
      };



      devEnv = [
        ghc-wrapped
        stack-wrapped
        hls
      ]
      ++ (with pkgs; [
        cabal-install
        hpack
        hlint
        nixpkgs-fmt
        ormolu
        nil
        rnix-lsp
      ])
      ++ (with hPkgs; [
        fourmolu
        implicit-hie
      ]);

      vscode = pkgs.vscode-with-extensions.override {
        # vscode = pkgs.vscode-fhsWithPackages (ps: devEnv ++ [ ps.zlib ]);
        vscodeExtensions = with pkgs.vscode-extensions;
          [
            haskell.haskell
            justusadam.language-haskell
            arrterian.nix-env-selector
            jnoortheen.nix-ide
            donjayamanne.githistory
            eamodio.gitlens

          ];
      };

    in
    {
      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [ zlib ];

        nativeBuildInputs = [ vscode ] ++ devEnv;

        withHoogle = true;
      };
    });
}
