;;; run-coverage.el --- Run tests with code coverage -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch script to run ERT tests with undercover.el coverage.
;; Usage: emacs -batch -L . -l scripts/run-coverage.el

;;; Code:

(require 'undercover)

(setq undercover-force-coverage t)

(undercover "chess-pos.el"
            "chess-ply.el"
            "chess-algebraic.el"
            "chess-fen.el"
            "chess-game.el"
            "chess-var.el"
            (:report-format 'lcov)
            (:report-file "coverage.lcov")
            (:send-report nil))

(require 'chess-perft)

(ert-run-tests-batch-and-exit "depth[123]")

;;; run-coverage.el ends here
