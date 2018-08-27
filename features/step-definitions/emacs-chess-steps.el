;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^ics session$"
       (lambda ()
         (let ((test-fn (expand-file-name (make-temp-name "test-fifo") (or small-temporary-file-directory temporary-file-directory))))
           (setq test-fifo (start-process-shell-command "test-fifo" nil (format "mkfifo %s && cat >> %s" test-fn test-fn)))
           (process-send-string test-fifo "login:\n")
           (process-send-string test-fifo "Logging you in as \"GuestME\"\n")
           (process-send-string test-fifo "fics% \n")
           (sleep-for 2)
           (chess-ics "nowhere.org" 5000 nil nil "sh" "-c" (format "cat %s" test-fn))
           
       )))

(When "^new game$"
      (lambda ()
        (process-send-string test-fifo (format "{Game 42 (GuestYOU vs. GuestME) Creating unrated blitz match.}\n"))
        (process-send-string test-fifo (format "<12> rnbqkbnr pppppppp -------- -------- -------- -------- PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 23 GuestYOU GuestME -1 5 5 39 39 300 300 1 none (0:00) none 1 0 0\n"))
        (process-send-string test-fifo (format "<12> rnbqkbnr pppppppp -------- -------- --P----- -------- PP-PPPPP RNBQKBNR B 2 1 1 1 1 0 23 GuestYOU GuestME 1 5 5 39 39 300 300 1 P/c2-c4 (0:00) c4 1 0 0\n"))
        (process-send-string test-fifo (format "<12> rnbqkbnr pppp-ppp -------- ----p--- --P----- -------- PP-PPPPP RNBQKBNR W 4 1 1 1 1 0 23 GuestYOU GuestME -1 5 5 39 39 300 300 2 P/e7-e5 (0:00) e5 1 1 0\n"))
        (sleep-for 2)
        )
      )
        
(When "^opponent forfeits on time$"
      (lambda ()
        (process-send-string test-fifo (format "{Game 42 (GuestYOU vs. GuestME) GuestYOU forfeits on time} 0-1\n"))
        (sleep-for 2)
        ))

(When "^opponent forfeits by disconnection$"
      (lambda ()
        (process-send-string test-fifo (format "{Game 42 (GuestYOU vs. GuestME) GuestYOU forfeits by disconnection} 1-0\n"))
        (sleep-for 2)
        ))

(When "^opponent aborts$"
      (lambda ()
        (process-send-string test-fifo (format "Your opponent has aborted the game on move one.\n"))
        (sleep-for 2)
        ))

(When "^I specify a good port$"
      (lambda ()
        (if espuds-chain-active
            (progn
              (setq espuds-action-chain (vconcat espuds-action-chain (string-to-vector test-port))))
          (execute-kbd-macro (string-to-vector test-port)))))

(Given "^game with fen \"\\(.+\\)\"$"
       (lambda (fen)
         (let ((game (chess-game-create (chess-fen-to-pos fen))) objects)
           (setq test-display (car (chess-create-modules (list '(chess-plain))
                                                         'chess--create-display
                                                         game t t))))))

(When "^I set position of \"\\(.+\\)\" to fen \"\\(.+\\)\"$"
      (lambda (process-name fen)
        (chess-with-current-buffer (process-buffer (get-process process-name))
          (chess-engine-set-position nil (chess-fen-to-pos fen)))
        (sleep-for 2)
))


(When "^I send position from \"\\(.+\\)\"$"
      (lambda (process-name)
        (chess-with-current-buffer (process-buffer (get-process process-name))
          (chess-game-run-hooks chess-module-game 'setup-pos (chess-game-pos chess-module-game)))
        (sleep-for 2)
))


(Then "^the move \"\\([a-h][1-8]\\)-\\([a-h][1-8]\\)\" is illegal$"
      (lambda (source target)
        (let ((position (chess-display-position test-display)))
          (assert (null (chess-ply-create position nil
                                          (chess-coord-to-index source)
                                          (chess-coord-to-index target)))))))

(Then "^I am ready to play$"
      (lambda ()
        (let ((ready (apply 'chess-string 'opp-ready (list user-full-name))))
          (Then "I should see message \"%s\"" ready))))

(When "^\\(white\\|black\\) moves \"\\(.+\\)\"$"
      (lambda (color move)
        (if (string= "white" color)
            (Given "I switch to buffer \"*Chessboard*\"")
          (Given "I switch to buffer \"*Chessboard*<2>\""))
        (When "I type \"%s\"" move)
        (sleep-for 1)
))

(When "^\\(white\\|black\\) selects \"\\([a-h][1-8]\\)\"$"
      (lambda (color source)
        (if (string= "white" color)
            (Given "I switch to buffer \"*Chessboard*\"")
          (Given "I switch to buffer \"*Chessboard*<2>\""))
        (When "I go to point \"%s\"" (number-to-string (chess-display-index-pos nil (chess-coord-to-index source))))
        (When "I press \"RET\"")
        (sleep-for 1)
))

(Then "^paint-move last \\([0-9]+\\) plies less than \\([0-9]+\\) microseconds"
      (lambda (times micros)
        (dotimes (i (string-to-number times))
          (let ((ply (chess-game-ply chess-module-game (1- (- chess-display-index i)))))
            (assert (< (measure-time (chess-display-paint-move nil ply))
                       (/ (string-to-number micros) 1e6)))))))

(Given "^I start server and client$"
       (lambda ()
         (Given "I start server")
         (sleep-for 1)
         (Given "I start client")
         (sleep-for 1)
         ))

(Given "^I start server$"
       (lambda ()
         (And "I start an action chain")
         (And "I press \"C-u M-x\"")
         (And "I type \"chess\"")
         (And "I press \"RET\"")
         (And "I type \"network\"")
         (And "I press \"RET\"")
         (And "I press \"s\"")
         (And "I specify a good port")
         (And "I press \"RET\"")
         (And "I execute the action chain")))

(Given "^I start client$"
       (lambda ()
         (And "I start an action chain")
         (And "I press \"C-u M-x\"")
         (And "I type \"chess\"")
         (And "I press \"RET\"")
         (And "I type \"network\"")
         (And "I press \"RET\"")
         (And "I press \"c\"")
         (And "I type \"localhost\"")
         (And "I press \"RET\"")
         (And "I specify a good port")
         (And "I press \"RET\"")
         (And "I execute the action chain")
))

(Then "^the square at \"\\([a-h][1-8]\\)\" is highlighted \\(.+\\)$"
      (lambda (source kind)
        (if (display-graphic-p)
            (let ((prop (copy-alist (get-text-property 
                                     (chess-display-index-pos nil (chess-coord-to-index source))
                                     'display))))
              (chess-display-highlight nil 
                                       (cond ((string= kind "selected")
                                              chess-images-highlight-color)
                                             ((string= kind "pre-move")
                                              chess-display-pre-move-color)
                                             ((string= kind "last-move")
                                              chess-display-last-move-color)
                                             ((string= kind "legal")
                                              chess-display-legal-move-color)
                                             (t chess-display-last-move-color))
                                       (chess-coord-to-index source))
              (assert (equal prop (get-text-property 
                                   (chess-display-index-pos nil (chess-coord-to-index source))
                                   'display))))
          (assert (eq (get-text-property 
                       (chess-display-index-pos nil (chess-coord-to-index source))
                       'face) (cond ((string= kind "selected") 'chess-ics1-highlight-face)
                                    (t 'chess-display-highlight)))))))

(Then "^the square at \"\\([a-h][1-8]\\)\" is unhighlighted$"
      (lambda (source)
        (if (display-graphic-p)
            (let ((prop (copy-alist (get-text-property 
                                     (chess-display-index-pos nil (chess-coord-to-index source))
                                     'display))))
              (chess-display-unhighlight-square nil (chess-coord-to-index source))
              (assert (equal prop (get-text-property 
                                   (chess-display-index-pos nil (chess-coord-to-index source))
                                   'display))))
          (assert (not (eq (get-text-property 
                            (chess-display-index-pos nil (chess-coord-to-index source))
                            'face) 'chess-display-highlight))))))
