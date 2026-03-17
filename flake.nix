{
  description = "Emacs Chess - a chess client and library for GNU Emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f {
        pkgs = nixpkgs.legacyPackages.${system};
        inherit system;
      });

      src = nixpkgs.lib.cleanSource ./.;

      emacsWithDev = pkgs: (pkgs.emacsPackagesFor pkgs.emacs-nox).emacsWithPackages (epkgs: with epkgs; [
        package-lint
        relint
        undercover
      ]);
    in
    {
      packages = forAllSystems ({ pkgs, ... }: {
        default = pkgs.stdenv.mkDerivation {
          pname = "emacs-chess";
          version = "2.0.1";
          inherit src;

          nativeBuildInputs = [ pkgs.emacs-nox pkgs.texinfo ];

          buildPhase = ''
            runHook preBuild

            # Generate autoloads
            emacs -batch -L . -l chess-maint.el \
              -f chess-generate-autoloads chess-auto.el .
            echo '(provide (quote chess-auto))' >> chess-auto.el

            # Generate ECO FEN table
            emacs --no-init-file --no-site-file -batch \
              -L . -l chess-eco -f chess-generate-fen-table \
              chess-eco.pos chess-eco.fen

            # Byte-compile
            emacs -batch -L . -f batch-byte-compile *.el

            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall

            mkdir -p $out/share/emacs/site-lisp/chess
            cp *.el *.elc $out/share/emacs/site-lisp/chess/
            cp chess-eco.pos chess-eco.fen chess-polyglot.bin \
              $out/share/emacs/site-lisp/chess/ 2>/dev/null || true
            cp -r pieces sounds $out/share/emacs/site-lisp/chess/ 2>/dev/null || true

            # Build texinfo documentation
            mkdir -p $out/share/info
            if [ -f doc/chess.texi ]; then
              makeinfo doc/chess.texi -o $out/share/info/chess.info
            fi

            runHook postInstall
          '';

          meta = {
            description = "Play chess in GNU Emacs";
            homepage = "https://github.com/jwiegley/emacs-chess";
            license = pkgs.lib.licenses.gpl3Plus;
          };
        };
      });

      checks = forAllSystems ({ pkgs, system, ... }:
        let
          emacs = pkgs.emacs-nox;
          emacsDevPkgs = emacsWithDev pkgs;
          copySrc = ''
            cp -a ${src}/. .
            chmod -R u+w .
          '';
        in
        {
          # Full package build
          build = self.packages.${system}.default;

          # Byte-compile with warnings as errors
          byte-compile = pkgs.runCommand "chess-byte-compile" {
            nativeBuildInputs = [ emacs ];
          } ''
            ${copySrc}

            # Generate autoloads (needed for compilation)
            emacs -batch -L . -l chess-maint.el \
              -f chess-generate-autoloads chess-auto.el . 2>/dev/null || true
            echo '(provide (quote chess-auto))' >> chess-auto.el

            emacs -batch -L . \
              --eval '(setq byte-compile-error-on-warn t)' \
              -f batch-byte-compile *.el

            touch $out
          '';

          # Lint with package-lint and relint
          lint = pkgs.runCommand "chess-lint" {
            nativeBuildInputs = [ emacsDevPkgs ];
          } ''
            ${copySrc}

            # Generate autoloads
            emacs -batch -L . -l chess-maint.el \
              -f chess-generate-autoloads chess-auto.el . 2>/dev/null || true
            echo '(provide (quote chess-auto))' >> chess-auto.el

            # Run package-lint on main entry point
            emacs -batch -L . \
              --eval '(require (quote package-lint))' \
              -f package-lint-batch-and-exit chess.el

            touch $out
          '';

          # Check indentation of all .el files
          format = pkgs.runCommand "chess-format-check" {
            nativeBuildInputs = [ emacs ];
          } ''
            ${copySrc}

            emacs -batch -L . \
              -l scripts/check-indent.el \
              -f chess-check-indent-batch *.el

            touch $out
          '';

          # Run ERT perft tests
          test = pkgs.runCommand "chess-test" {
            nativeBuildInputs = [ emacs ];
          } ''
            ${copySrc}

            # Generate autoloads
            emacs -batch -L . -l chess-maint.el \
              -f chess-generate-autoloads chess-auto.el . 2>/dev/null || true
            echo '(provide (quote chess-auto))' >> chess-auto.el

            emacs -batch -L . \
              -l chess-perft.el \
              --eval '(ert-run-tests-batch-and-exit "depth[123]")'

            touch $out
          '';

          # Code coverage report
          coverage = pkgs.runCommand "chess-coverage" {
            nativeBuildInputs = [ emacsDevPkgs pkgs.lcov ];
          } ''
            ${copySrc}

            # Generate autoloads
            emacs -batch -L . -l chess-maint.el \
              -f chess-generate-autoloads chess-auto.el . 2>/dev/null || true
            echo '(provide (quote chess-auto))' >> chess-auto.el

            mkdir -p $out

            emacs -batch -L . \
              -l scripts/run-coverage.el \
              2>&1 || true

            if [ -f coverage.lcov ]; then
              cp coverage.lcov $out/
              genhtml coverage.lcov -o $out/html 2>/dev/null || true
            fi
          '';

          # Performance benchmark using perft
          perft-benchmark = pkgs.runCommand "chess-perft-benchmark" {
            nativeBuildInputs = [ emacs ];
          } ''
            ${copySrc}

            # Generate autoloads
            emacs -batch -L . -l chess-maint.el \
              -f chess-generate-autoloads chess-auto.el . 2>/dev/null || true
            echo '(provide (quote chess-auto))' >> chess-auto.el

            mkdir -p $out

            emacs -batch -L . \
              -l chess-perft.el \
              --eval '(let ((start (float-time)))
                        (ert-run-tests-batch "depth[123]")
                        (with-temp-file "perft-timing.txt"
                          (insert (format "Total perft time: %.2f seconds\n"
                                          (- (float-time) start)))))' \
              2>&1 | tee $out/perft-results.txt

            cp perft-timing.txt $out/ 2>/dev/null || true
          '';

          # Build texinfo documentation
          docs = pkgs.runCommand "chess-docs" {
            nativeBuildInputs = [ pkgs.texinfo ];
          } ''
            ${copySrc}
            mkdir -p $out
            makeinfo doc/chess.texi -o $out/chess.info
          '';
        });

      devShells = forAllSystems ({ pkgs, ... }: {
        default = pkgs.mkShell {
          buildInputs = [
            (emacsWithDev pkgs)
            pkgs.lcov
            pkgs.lefthook
            pkgs.texinfo
          ];
          shellHook = ''
            echo "Emacs Chess development shell"
            echo ""
            echo "  lefthook install     Set up pre-commit hooks"
            echo "  nix flake check      Run all checks"
            echo "  nix build            Build the package"
            echo ""
          '';
        };
      });
    };
}
