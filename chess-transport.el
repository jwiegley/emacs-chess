;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; An example of a generic transport engine, based on the protocol
;; used by chess-network.el.  The only parts missing are send and
;; receive.  This could be used for transmitting chess.el protocol
;; over CTCP, for example.
;;
;; $Revision$

(require 'chess-network)

(defalias 'chess-network-regexp-alist 'chess-transport-regexp-alist)

(defun chess-transport-handler (event &rest args)
  "This is an example of a generic transport engine."
  (cond
   ((eq event 'initialize)
    ;; Initialize the transport here, if necessary.  Make sure that
    ;; any housekeeping data you use is kept in buffer-local
    ;; variables.  Otherwise, multiple games played using the same
    ;; kind of transport might collide.  For example:
    ;;
    ;; (set (make-local-variable 'chess-transport-data) (car args))
    )

   ((eq event 'send)
    ;; Transmit the string given in `(car args)' to the outbound
    ;; transport from here
    )))

;; Call `(chess-engine-submit engine STRING)' for text that arrives
;; from the inbound transport

(provide 'chess-transport)

;;; chess-transport.el ends here