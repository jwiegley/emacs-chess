(require 'f)

(defvar emacs-chess-support-path
  (f-dirname load-file-name))

(defvar emacs-chess-features-path
  (f-parent emacs-chess-support-path))

(defvar emacs-chess-root-path
  (f-parent emacs-chess-features-path))

;; Nikolaj Schumacher
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(add-to-list 'load-path emacs-chess-root-path)

(require 'chess-ics)
(require 'chess)
(require 'espuds)
(require 'ert)
(require 'cl)

(Setup
 (defvar test-display nil)
 (defvar test-fifo nil)
 (defvar test-port nil)
 (setq test-port (catch 'loop
                   (dolist (cand (list "5678" "5413" "5142" "5308" "5987"))
                     (when (= 1 (call-process "nc" nil nil nil "-z" "localhost" cand))
                       (throw 'loop cand)))))
 (setq noninteractive t)
 (custom-set-variables '(chess-sound-moves nil)
                       '(chess-display-highlight-last-move t)
                       '(chess-display-highlight-legal t)
                       '(chess-display-popup nil)
                       '(chess-display-allow-pre-moves t)
                       '(chess-images-separate-frame nil))
)

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 (when test-display 
   (chess-module-destroy test-display)
   (setq test-display nil))
 (dolist (p (process-list))
   (delete-process p))
 (setq test-fifo nil)
 (let ((buf (get-buffer "*Chessboard*"))
       (buf2 (get-buffer "*Chessboard*<2>")))
   (if buf (kill-buffer buf))
   (if buf2 (kill-buffer buf2)))
)

(Teardown
 ;; After when everything has been run
 )
