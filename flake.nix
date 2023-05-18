{
  description = "my project description";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk/master";
    fenix.url = "github:nix-community/fenix";
    fenix.inputs.nixpkgs.follows = "nixpkgs";
    gtk4-layer-shell-src.url = "github:wmww/gtk4-layer-shell";
    gtk4-layer-shell-src.flake = false;
    hls-flake.url = "git+https://github.com/haskell/haskell-language-server?tag=1.10.0";
  };

  outputs = { self, nixpkgs, flake-utils, naersk, fenix, gtk4-layer-shell-src, hls-flake }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = { allowUnfree = true; };
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

          mesonFlags = [ "-Dintrospection=true" ];
        };
        naersk' = pkgs.callPackage naersk { };

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
          overrides = self: super: { widgets = widgets-rust; };
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with pkgs;
              [ cabal-install
                ghcid
              ]);
        };

        hPkgs = pkgs.haskellPackages.extend (self: super: {widgets = widgets;});
        rPkgs = fenix.packages."${system}";

        ghc-wrapped = hPkgs.ghcWithHoogle (hPkgs: with hPkgs; [ 
          widgets 
          megaparsec
          containers
          transformers
        ]);

        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };

        rustDevEnv = (with pkgs; [
          gtk4-layer-shell
          pkg-config

          gtk4
          pango
          glib
          harfbuzz
          cairo
          gdk-pixbuf
          graphene
        ]) ++ (with rPkgs; [
          rust-analyzer
          stable.completeToolchain
        ]);

        haskellDevEnv = [
          ghc-wrapped
          stack-wrapped
        ] ++ (with hPkgs; [
          implicit-hie
          retrie
        ]) ++ (with pkgs; [
          zlib
          haskell-language-server
          hlint
          hpack
          cabal-install
          ormolu
          ocamlPackages.ocaml-lsp
          ocaml
        ]);

        vscode = pkgs.vscode-with-extensions.override {
          vscode = pkgs.vscode.fhsWithPackages (ps: rustDevEnv ++ haskellDevEnv);
          vscodeExtensions = with pkgs.vscode-extensions; [
            haskell.haskell
            justusadam.language-haskell
            matklad.rust-analyzer
            ocamllabs.ocaml-platform
          ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
            {
              name = "hoogle-vscode";
              publisher = "jcanero";
              version = "0.0.7";
              sha256 = "sha256-QU2psApUsSd70Lol6FbQopoT5x/raA5FOgLJsbO7qlk=";
            }
          ];
        };


        myDevTools = haskellDevEnv ++ rustDevEnv ++ [vscode];
      in {
        packages.default = pkgs.haskellPackages.developPackage {
          root = ./.;
          overrides = self: super: { widgets = widgets; };
        };
      
        devShells.default = pkgs.mkShell {
          buildInputs = myDevTools;
        };
      });
}
