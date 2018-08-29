(require 'f)

(defvar emacs-chess-support-path
  (f-dirname load-file-name))

(defvar emacs-chess-features-path
  (f-parent emacs-chess-support-path))

(defvar emacs-chess-root-path
  (f-parent emacs-chess-features-path))

(add-to-list 'load-path emacs-chess-root-path)

(require 'chess-ics)
(require 'espuds)
(require 'ert)
(require 'cl)

(Setup
 (defvar test-display nil)
 (defvar test-fifo nil)
)

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 (when test-display 
   (chess-module-destroy test-display)
   (setq test-display nil))
 (when (process-status "*chess-ics*")
   (delete-process "*chess-ics*"))
 (when test-fifo 
   (process-send-eof test-fifo)
   (delete-process test-fifo)
   (setq test-fifo nil))
)

(Teardown
 ;; After when everything has been run
 )
