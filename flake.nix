{
  description = "my project description";
  inputs = {
    system.url = git+file:/etc/nixos;
    nixpkgs.follows = "system/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk/master";
    fenix.url = "github:nix-community/fenix";
    fenix.inputs.nixpkgs.follows = "nixpkgs";
    gtk4-layer-shell-src.url = "github:wmww/gtk4-layer-shell";
    gtk4-layer-shell-src.flake = false;
    hls-flake.url = "git+https://github.com/haskell/haskell-language-server?tag=1.10.0";
    lean.url = "github:leanprover/lean4";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    naersk,
    fenix,
    gtk4-layer-shell-src,
    hls-flake,
    lean,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        config = {allowUnfree = true;};
      };

      gtk4-layer-shell = pkgs.stdenv.mkDerivation {
        name = "gtk4-layer-shell";
        version = "1.0";

        src = gtk4-layer-shell-src;

        buildInputs = with pkgs; [
          meson
          ninja
          wayland
          gtk4
          gobject-introspection
          pkg-config
          vala
        ];

        mesonFlags = ["-Dintrospection=true"];
      };

      rPkgs = fenix.packages."${system}";

      rustToolchain = rPkgs.default.toolchain;

      naersk' = pkgs.callPackage naersk {
        cargo = rustToolchain;
        rustc = rustToolchain;
      };

      widgets-rust = naersk'.buildPackage {
        src = ./widgets/.;
        copyLibs = true;

        nativeBuildInputs = [
          pkgs.ghc
        ];

        buildInputs = with pkgs; [
          gtk4-layer-shell
          pkg-config

          gtk4
          pango
          glib
          harfbuzz
          cairo
          gdk-pixbuf
          graphene
          gtk4-layer-shell
        ];
      };

      widgets = pkgs.haskellPackages.developPackage {
        root = ./widgets/.;
        overrides = self: super: {widgets = widgets-rust;};
        modifier = drv:
          pkgs.haskell.lib.addBuildTools drv (with pkgs; [
            cabal-install
            ghcid
          ]);
      };

      hPkgs = pkgs.haskellPackages.extend (self: super: {widgets = widgets;});

      ghc-wrapped = hPkgs.ghcWithHoogle (hPkgs:
        with hPkgs; [
          # widgets
          megaparsec
          containers
          transformers
          lens
          HUnit
        ]);

      stack-wrapped = pkgs.symlinkJoin {
        name = "stack"; # will be available as the usual `stack` in terminal
        paths = [pkgs.stack];
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

      rustDevEnv =
        (with pkgs; [
          gtk4-layer-shell
          pkg-config

          gtk4
          pango
          glib
          harfbuzz
          cairo
          gdk-pixbuf
          graphene
        ])
        ++ (with rPkgs; [
          rust-analyzer
          default.toolchain
        ]);

      haskellDevEnv =
        [
          ghc-wrapped
          stack-wrapped
        ]
        ++ (with hPkgs; [
          implicit-hie
          retrie
          weeder
        ])
        ++ (with pkgs; [
          zlib
          haskell-language-server
          hlint
          hpack
          cabal-install
          ormolu
        ]);

      ocamlDevEnv = with pkgs; [
        ocamlPackages.ocaml-lsp
        ocaml
      ];

      leanDevEnv = [lean.packages."${system}".lean-all];

      vscode = pkgs.vscode-with-extensions.override {
        vscode = pkgs.vscode.fhsWithPackages (ps: rustDevEnv ++ haskellDevEnv ++ ocamlDevEnv);
        vscodeExtensions = with pkgs.vscode-extensions;
          [
            haskell.haskell
            justusadam.language-haskell
            matklad.rust-analyzer
            ocamllabs.ocaml-platform
            vadimcn.vscode-lldb
            usernamehw.errorlens
            rust-lang.rust-analyzer
            mkhl.direnv
            mhutchie.git-graph
            kahole.magit
            kamadorueda.alejandra
            jnoortheen.nix-ide
            eamodio.gitlens
            arrterian.nix-env-selector
          ]
          ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
            {
              name = "hoogle-vscode";
              publisher = "jcanero";
              version = "0.0.7";
              sha256 = "sha256-QU2psApUsSd70Lol6FbQopoT5x/raA5FOgLJsbO7qlk=";
            }
            {
              name = "lean4";
              publisher = "leanprover";
              version = "0.0.102";
              sha256 = "sha256-anClL+2qmZbgXeqQdm18PITcFGKKhp+Qs3sU6nx2agw=";
            }
          ];
      };

      myDevTools = haskellDevEnv ++ rustDevEnv ++ ocamlDevEnv ++ [vscode];
    in {
      packages.default = pkgs.haskellPackages.developPackage {
        root = ./.;
        overrides = self: super: {widgets = widgets;};
      };

      devShells.default = pkgs.mkShell {
        buildInputs = myDevTools;
      };
    });
}
