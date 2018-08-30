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
           (sleep-for 3)
           (chess-ics "nowhere.org" 5000 nil nil "sh" "-c" (format "cat %s" test-fn))
           
       )))

(When "^new game$"
      (lambda ()
        (process-send-string test-fifo (format "{Game 42 (GuestYOU vs. GuestME) Creating unrated blitz match.}\n"))
        (process-send-string test-fifo (format "<12> rnbqkbnr pppppppp -------- -------- -------- -------- PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 23 GuestYOU GuestME -1 5 5 39 39 300 300 1 none (0:00) none 1 0 0\n"))
        (process-send-string test-fifo (format "<12> rnbqkbnr pppppppp -------- -------- --P----- -------- PP-PPPPP RNBQKBNR B 2 1 1 1 1 0 23 GuestYOU GuestME 1 5 5 39 39 300 300 1 P/c2-c4 (0:00) c4 1 0 0\n"))
        (process-send-string test-fifo (format "<12> rnbqkbnr pppp-ppp -------- ----p--- --P----- -------- PP-PPPPP RNBQKBNR W 4 1 1 1 1 0 23 GuestYOU GuestME -1 5 5 39 39 300 300 2 P/e7-e5 (0:00) e5 1 1 0\n"))
        (sleep-for 3)
        )
      )
        
(When "^opponent forfeits on time$"
      (lambda ()
        (process-send-string test-fifo (format "{Game 42 (GuestYOU vs. GuestME) GuestYOU forfeits on time} 0-1\n"))
        (sleep-for 3)
        ))

(Given "^game with fen \"\\(.+\\)\"$"
       (lambda (fen)
         (let ((game (chess-game-create (chess-fen-to-pos fen))) objects)
           (setq test-display (car (chess-create-modules (list '(chess-plain))
                                                         'chess--create-display
                                                         game t t))))))

(Then "^the move \"\\([a-h][1-8]\\)-\\([a-h][1-8]\\)\" is illegal$"
      (lambda (source target)
        (let ((position (chess-display-position test-display)))
          (assert (null (chess-ply-create position nil
                                          (chess-coord-to-index source)
                                          (chess-coord-to-index target)))))))


