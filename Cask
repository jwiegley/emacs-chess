;;-*- Mode: Emacs-Lisp -*-
;;  Cask is a package manager for emacs lisp projects.  It runs
;;  tests and can generate the _pkg.el file.
;;
;;  See http://cask.readthedocs.org/en/latest/guide/dsl.html for more
;;  information about Cask.
;;
;;    cask pkg-file
;;
;;    cask exec ecukes
;;    cask install
;;
;;  are particularly useful commands (update/install is for flycheck-cask).
;;
;;; Code:

(source gnu)
(source melpa)

(package-file "chess.el")

(development
  ;; optional dependencies (used in the tests)
  (depends-on "f")
  (depends-on "ert-runner")
  (depends-on "ecukes")
  (depends-on "espuds"))

;;; Cash ends here
