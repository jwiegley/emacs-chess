;;; check-indent.el --- Check Emacs Lisp indentation -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch script to verify that .el files are properly indented.
;; Usage: emacs -batch -l scripts/check-indent.el -f chess-check-indent-batch FILE...

;;; Code:

(defun chess-check-indent-batch ()
  "Check indentation of files listed on the command line."
  (let ((errors 0))
    (dolist (file command-line-args-left)
      (when (and (file-exists-p file)
                 (string-suffix-p ".el" file))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((original (buffer-string)))
            (emacs-lisp-mode)
            (indent-region (point-min) (point-max))
            (unless (string= original (buffer-string))
              (message "Indentation differs in %s" file)
              (setq errors (1+ errors)))))))
    (setq command-line-args-left nil)
    (when (> errors 0)
      (message "%d file(s) with indentation issues" errors)
      (kill-emacs 1))))

(defun chess-fix-indent-batch ()
  "Fix indentation of files listed on the command line."
  (dolist (file command-line-args-left)
    (when (and (file-exists-p file)
               (string-suffix-p ".el" file))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((original (buffer-string)))
          (emacs-lisp-mode)
          (indent-region (point-min) (point-max))
          (unless (string= original (buffer-string))
            (write-region (point-min) (point-max) file)
            (message "Fixed indentation in %s" file))))))
  (setq command-line-args-left nil))

(provide 'check-indent)
;;; check-indent.el ends here
