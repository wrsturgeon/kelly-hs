{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };
  outputs =
    {
      flake-utils,
      nix-filter,
      nixpkgs,
      self,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pname = "kelly-hs";
        synopsis = "Kelly criterion";
        description = synopsis;

        # https://pvp.haskell.org
        #          +-+------- breaking API changes
        #          | | +----- non-breaking API additions
        #          | | | +--- code changes with no API change
        version = "0.1.0.0";

        pkgs = import nixpkgs { inherit system; };
        hs-pkgs = pkgs.haskellPackages;

        ENV = {
          # environment variables
        };

        src-dir-name = "src";
        app-dir-name = "app";
        test-dir-name = "test";
        cabal-file = "${pname}.cabal";
        cabal-version =
          let
            full-version = hs-pkgs.cabal-install.version;
            split = pkgs.lib.strings.splitString "." full-version;
            truncated = pkgs.lib.lists.take 2 split;
          in
          # pkgs.lib.strings.concatStringsSep "." truncated;
          "3.8";
        haskell-standard = "GHC2021"; # "Haskell2010";
        main-library-module = "Kelly";
        disabled-warnings = [
          "all-missed-specialisations"
          "implicit-prelude"
          "missing-safe-haskell-mode"
          "prepositive-qualified-module"
          "safe"
          "unsafe"
        ];
        language-extensions = [
          "LinearTypes"
        ];
        disabled-language-extensions = [
          "ImplicitPrelude"
          "GeneralizedNewtypeDeriving"
        ];
        common-deps = [ "linear-base" ];
        src-deps = [ "base" ];
        app-deps = [ "base" ];
        test-deps = [ ];
        cabal-contents = for-nix-only: ''
          cabal-version: ${cabal-version}

          name: ${pname}
          version: ${version}

          synopsis: ${synopsis}
          description: ${description}
          homepage: https://github.com/wrsturgeon/${pname}
          category: Math
          ${if for-nix-only then "" else "extra-doc-files: README.md"}

          license: MPL-2.0

          author: Will Sturgeon
          maintainer: willstrgn@gmail.com

          build-type: Simple

          common extensions
              default-extensions: ${
                pkgs.lib.concatStringsSep ", " (
                  language-extensions ++ builtins.map (extn: "No${extn}") disabled-language-extensions
                )
              }

          common warnings
              ghc-options: -Wall -Werror -Weverything ${
                pkgs.lib.strings.concatStringsSep " " (builtins.map (w: "-Wno-${w}") disabled-warnings)
              }

          library
              import: extensions, warnings
              exposed-modules: ${main-library-module}
              other-modules: ${
                pkgs.lib.strings.concatStringsSep ", " (
                  builtins.filter (mod: mod != main-library-module) (
                    builtins.map (pkgs.lib.strings.removeSuffix ".hs") (
                      builtins.filter (pkgs.lib.strings.hasSuffix ".hs") (
                        builtins.attrNames (builtins.readDir ./${src-dir-name})
                      )
                    )
                  )
                )
              }
              build-depends:
                  ${pkgs.lib.strings.concatStringsSep ", " (common-deps ++ src-deps)}
              hs-source-dirs: ${src-dir-name}
              default-language: ${haskell-standard}

          executable ${pname}
              import: extensions, warnings
              main-is: Main.hs
              build-depends:
                  ${pkgs.lib.strings.concatStringsSep ", " ([ pname ] ++ common-deps ++ app-deps)}
              hs-source-dirs: ${app-dir-name}
              default-language: ${haskell-standard}

          test-suite ${pname}-test
              import: extensions, warnings
              default-language: ${haskell-standard}
              type: exitcode-stdio-1.0
              hs-source-dirs: ${test-dir-name}
              main-is: Main.hs
              build-depends:
                  ${pkgs.lib.strings.concatStringsSep ", " ([ pname ] ++ common-deps ++ test-deps)}
        '';
        update-cabal = for-nix-only: ''
          echo ${pkgs.lib.strings.escapeShellArg (cabal-contents for-nix-only)} > ${cabal-file}
        '';

        src = nix-filter {
          root = ./.;
          include = [
            ./${app-dir-name}
            ./${src-dir-name}
            ./${test-dir-name}
          ];
        };
        full-src = pkgs.stdenvNoCC.mkDerivation {
          pname = "full-src";
          inherit version;
          inherit src;
          buildPhase = update-cabal true;
          installPhase = "cp -r . $out";
        };

      in
      {
        apps.update-cabal = {
          type = "app";
          program =
            let
              script = ''
                #!${pkgs.bash}/bin/bash
                set -eu
                ${update-cabal false}
              '';
              written = pkgs.writeScriptBin pname script;
            in
            "${written}/bin/${pname}";
        };
        packages.default = hs-pkgs.developPackage (
          ENV
          // {
            root = full-src;

            modifier =
              drv:
              pkgs.haskell.lib.addBuildTools drv (
                with hs-pkgs;
                [
                  cabal-install
                  haskell-language-server
                ]
              );
          }
        );
        devShells.default = pkgs.mkShell (
          ENV
          // {
            inputsFrom = builtins.attrValues self.packages.${system};
            packages = with hs-pkgs; [
              hoogle
              ormolu
            ];
          }
        );
      }
    );
}
