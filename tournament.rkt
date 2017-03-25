#lang racket

(require mzlib/struct)
(require "GomokuServer.rkt")


;; define racket executable for process*
(define racket (find-executable-path "racket"))


;; reset-player: player -> void
(define (reset-player p)
  (set-player-wins! p 0)
  (set-player-losses! p 0)
  (set-player-draws! p 0))

;; drain-all-process-ports: player process-list -> (void)
;; drain the process standard-out and standard-error ports and display
(define (drain-all-process-ports p pl)
  ;(when (symbol=? ((fifth pl) 'status) 'running)
    ;((fifth pl) 'interrupt)
    (printf "~a's standard out~%" (player-name p))
    (if (and (port? (first pl)) (byte-ready? (first pl)))
        (for ([l (in-port read-line (first pl))]) (displayln l))
        (printf " -- nothing to report --~%"))
    (printf "~a's standard error~%" (player-name p))
    (if (and (port? (fourth pl)) (byte-ready? (fourth pl)))
        (for ([l (in-port read-line (fourth pl))]) (displayln l))
        (printf " -- nothing to report --~%"))
    (printf "~%");)
  )

;; close-all-ports: player process-list -> (void)
;; close the ports used by the socket as well as the ports from the process
(define (close-all-ports p pl)
  (close-output-port (player-oprt p)) ; socket writing
  (close-input-port (player-iprt p))  ; socket reading
  (when (symbol=? ((fifth pl) 'status) 'running)
    (close-input-port (first pl))     ; process output
    (close-output-port (second pl))   ; process input
    (close-input-port (fourth pl))    ; process error
  ))

;; inc-w/l/d : player -> void
;; increment the RESULTS hash for the given player
(define (inc-w/l/d p)
  (let ([score-vec (hash-ref RESULTS (player-name p))])
    (vector-set! score-vec 0 (+ (player-wins p) (vector-ref score-vec 0)))
    (vector-set! score-vec 1 (+ (player-losses p) (vector-ref score-vec 1)))
    (vector-set! score-vec 2 (+ (player-draws p) (vector-ref score-vec 2)))))
    

;; tournament-games: int player player tcp-listener -> ...
;; run num-games games between the two players with given names, finally reporting the results
(define (tournament-games num-games p1 p2 my-listener)
  (cond [(or (> (player-wins p1) (floor (/ num-games 2)))
             (> (player-wins p2) (floor (/ num-games 2))))
         (printf "~%Tournament Pairing Result~%Player: ~a won ~a~%Player: ~a won ~a~%~%"
                 (player-name p1) (player-wins p1) (player-name p2) (player-wins p2))
         (inc-w/l/d p1) (inc-w/l/d p2)
         (reset-player p1) (reset-player p2)
         ]
        [else
         (printf "==========================================~%")
         (printf "Play a game: ~a as black and ~a as white~%" (player-name p1) (player-name p2))
         (printf "==========================================~%")
         (let*-values ([(start-p1) (parameterize ([subprocess-group-enabled #t])
                                     (process (player-command p1)))]
                       [(p1-iprt p1-oprt) (tcp-accept my-listener)]
                       [(start-p2) (parameterize ([subprocess-group-enabled #t])
                                     (process (player-command p2)))]
                       [(p2-iprt p2-oprt) (tcp-accept my-listener)]
                       )
           (reset-start-game)
           (set-player-iprt! p1 p1-iprt) (set-player-oprt! p1 p1-oprt) (set-player-proclist! p1 start-p1)
           (set-player-iprt! p2 p2-iprt) (set-player-oprt! p2 p2-oprt) (set-player-proclist! p2 start-p2)
           ;(nu-srv-game START-GAME p1 p2 'x)
           (srv-game START-GAME p1 p2 'x)
           ;; start cleaning up
           (drain-all-process-ports p1 start-p1)
           (drain-all-process-ports p2 start-p2)
           (printf "~%") (printf "~%")
           (close-all-ports p1 start-p1)
           (close-all-ports p2 start-p2)
           ((fifth start-p1) 'kill)
           ((fifth start-p2) 'kill)
           ;(tcp-close my-listener)
           (tournament-games num-games p2 p1 my-listener))]))
      

(define players
  (list
   (player (void) (void) "ABsearch" "cd /home/iba/teaching/tmp/CS455ai/ ; racket ABGomoku.rkt" 0 0 0 empty)
   ;(player (void) (void) "Alt-ABsearch" "cd /home/iba/teaching/tmp/CS455ai/ ; racket AltABGomoku.rkt" 0 0 0 empty)
   ;(player (void) (void) "random1" "RandomPlayer.rkt" 0 0 0 empty)
   ;(player (void) (void) "random2" "RandomPlayer.rkt" 0 0 0 empty)
   ;(player (void) (void) "group1" "cd /home/iba/teaching/tmp/CS455ai/gomoku/group1/src/ ; java GomokuPlayer" 0 0 0 empty)
   ;(player (void) (void) "group2" "cd /home/iba/teaching/tmp/CS455ai/gomoku/group2/ ; java GomokuClient" 0 0 0 empty)
   ;(player (void) (void) "group3" "cd /home/iba/teaching/tmp/CS455ai/gomoku/group3/src/ ; java GomokuDriver" 0 0 0 empty)
   ;(player (void) (void) "group4" "cd /home/iba/teaching/tmp/CS455ai/gomoku/group4/GomokuAgent/src/ ; java Main" 0 0 0 empty)
   (player (void) (void) "group5" "cd /home/iba/teaching/tmp/CS455ai/gomoku/group5/src/ ; java GomokuAgent" 0 0 0 empty)
   ;(player (void) (void) "group6" "cd /home/iba/teaching/tmp/CS455ai/gomoku/group6/src/ ; java GomokuAgent" 0 0 0 empty)
   ;;;;(player (void) (void) "group7" "cd /home/iba/teaching/tmp/CS455ai/gomoku/group7... ; ..." 0 0 0 empty)
   (player (void) (void) "group8" "cd /home/iba/teaching/tmp/CS455ai/gomoku/group8/ ; java GomokuClient" 0 0 0 empty)
   ))

(define RESULTS (for/hash ([p players]) (values (player-name p) (vector 0 0 0))))

(define my-listener (get-a-listener))
;#|
(do ([ps players (cdr ps)])
  ((= (length ps) 1))
  (for ([p2 (cdr ps)])
    (let ([p1 (car ps)])
      (tournament-games 9 p1 p2 my-listener))))
;|#

;; just test the first player against all the others
;(for ([p (cdr players)]) (tournament-games 7 (car players) p my-listener))


;; finally, report the results
(printf "OVERALL RESULTS FOR THE TOURNAMENT~%==================================~%")
(for ([p players])
  (let ([res (hash-ref RESULTS (player-name p))])
    (printf "player ~a: win:~a / lose:~a / draw:~a~%"
            (player-name p) (vector-ref res 0) (vector-ref res 1) (vector-ref res 2))))

  
(tcp-close my-listener)
(stop-draw)
