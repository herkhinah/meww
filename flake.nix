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
  };

  outputs = { self, nixpkgs, flake-utils, naersk, fenix, gtk4-layer-shell-src }:
    flake-utils.lib.eachDefaultSystem (system:
      let
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

            haskell.compiler.ghc927
          ];
        };
        # This overlay adds our project to pkgs
        widgets = pkgs.haskellPackages.developPackage {
          root = ./widgets/.;
          overrides = self: super: { widgets = widgets-rust; };
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
              [ cabal-install
                ghcid
              ]);
        };

        pkgs = import nixpkgs {
          inherit system;
        };

        hPkgs = pkgs.haskellPackages;
        rPkgs = fenix.packages."${system}";

        myDevTools = [
          (hPkgs.ghcWithPackages (pkgs: [ 
            widgets 
          ] )) # GHC compiler in the desired version (will be available on PATH)
          hPkgs.ghcid # Continuous terminal Haskell compile checker
          hPkgs.ormolu # Haskell formatter
          hPkgs.hlint # Haskell codestyle checker
          hPkgs.hoogle # Lookup Haskell documentation
          hPkgs.haskell-language-server # LSP server for editor
          hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
          hPkgs.retrie # Haskell refactoring tool
          hPkgs.hpack
          # hPkgs.cabal-install
          stack-wrapped
          pkgs.zlib # External C library needed by some Haskell packages

          rPkgs.rust-analyzer
          rPkgs.stable.completeToolchain
        ];

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
      in {
        packages.default = pkgs.haskellPackages.developPackage {
          root = ./.;
          overrides = self: super: { widgets = widgets; };
        };
      
        devShells.default = pkgs.mkShell {
          buildInputs = myDevTools;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          # LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
        };
      });
}
