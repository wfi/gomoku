#lang racket

(require "GomokuServer.rkt")


;; reset-player: player -> void
(define (reset-player p)
  (set-player-wins! p 0)
  (set-player-losses! p 0)
  (set-player-draws! p 0))

;; nu-srv-game: GS player player symbol -> ...
(define (nu-srv-game gs p1 p2 to-play)
  (draw-game gs)
  ;; unless game-over, do:
  (cond [(not (no-valid-moves? gs))
         ;; send game-status, board-state, and player-to-play to p1
         (send-game-info 'continuing gs to-play (player-oprt p1)) (flush-output (player-oprt p1))
         (let* ([start-time (current-milliseconds)]
                [a-move (read-move (player-iprt p1))]) ; read move from p1
           ;; check if 
           (cond [(> (- (current-milliseconds) start-time) (* 1000 MAX-MOVE-TIME))
                  (send-game-info 'forfeit-time gs to-play (player-oprt p1))
                  (update-score p1 'lose)
                  (send-game-info 'win gs (toggle to-play) (player-oprt p2))
                  ;(printf "nu-srv-game: adding 1 to score for ~a will have ~a~%" (player-name pt2) (player-score pt2))
                  (update-score p2 'win)]
                 [(not (and (number? (car a-move)) (< -1 (car a-move) BOARD-SIZE)
                            (number? (cdr a-move)) (< -1 (cdr a-move) BOARD-SIZE)
                            (symbol=? 'b (vgame-spot gs (car a-move) (cdr a-move)))))
                  (send-game-info 'forfeit-move gs to-play (player-oprt p1))
                  (update-score p1 'lose)
                  (send-game-info 'win gs (toggle to-play) (player-oprt p2))
                  (update-score p2 'win)]
                 [else ;; move is valid (i.e., on the board and vacant)
                  (place-move! gs a-move to-play) ; and update game-state accordingly
                  ;; call srv-game with new game-state and p2 p1 swapped
                  (nu-srv-game gs p2 p1 (toggle to-play))]))]
        [else ;; terminate with actual outcome
         (let ([outcome-p1 (win/lose/draw gs to-play)]
               [outcome-p2 (win/lose/draw gs (toggle to-play))])
           ;(printf "nu-srv-game: in final else with outcome for ~a given by win/lose/draw as: ~a~%" to-play outcome-p1)
           (send-game-info outcome-p1 gs to-play (player-oprt p1)) (flush-output (player-oprt p1))
           (send-game-info outcome-p2 gs to-play (player-oprt p2)) (flush-output (player-oprt p2))
           (update-score p1 outcome-p1)
           (update-score p2 outcome-p2))]))

;; drain-all-process-ports: player process-list -> (void)
;; drain the process standard-out and standard-error ports and display
(define (drain-all-process-ports p pl)
  (when (symbol=? ((fifth pl) 'status) 'running)
    (printf "~a's standard out~%" (player-name p))
    (for ([l (in-port read-line (first pl))]) (displayln l))
    (printf "~a's standard error~%" (player-name p))
    (for ([l (in-port read-line (fourth pl))]) (displayln l))
    (printf "~%")))

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

;; tournament-games: int player player tcp-listener -> ...
;; run num-games games between the two players with given names, finally reporting the results
(define (tournament-games num-games p1 p2 my-listener)
  (cond [(or (> (player-wins p1) (floor (/ num-games 2)))
             (> (player-wins p2) (floor (/ num-games 2))))
         (printf "~%Tournament Pairing Result~%Player: ~a won ~a~%Player: ~a won ~a~%~%"
                 (player-name p1) (player-wins p1) (player-name p2) (player-wins p2))
         (reset-player p1) (reset-player p2)
         ]
        [else 
         (printf "Let's play a game~%")
         (let*-values ([(start-p1) (process (player-command p1))]
                       [(p1-iprt p1-oprt) (tcp-accept my-listener)]
                       [(start-p2) (process (player-command p2))]
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
   (player (void) (void) "random1" "racket RandomPlayer.rkt" 0 0 0 empty)
   (player (void) (void) "random2" "racket RandomPlayer.rkt" 0 0 0 empty)
   (player (void) (void) "random3" "racket RandomPlayer.rkt" 0 0 0 empty)
   ;(player (void) (void) "chris" "cd /home/iba/teaching/CS116ai/spring16/gamesearch/cbetsillP03/; java gomoku" 0 0 0)
   ;(player (void) (void) "hunter" "cd /home/iba/teaching/CS116ai/spring16/gamesearch/hmcgushionP3/; java Controller3AlphaBeta04" 0 0 0)
   ;(player (void) (void) "james" "cd /home/iba/teaching/CS116ai/spring16/gamesearch/jbyronP3/; java GomokuAgent" 0 0 0)
   ;(player (void) (void) "jacob" "cd /home/iba/teaching/CS116ai/spring16/gamesearch/jochsP3/; java GomokuSearch" 0 0 0)
   ;(player (void) (void) "jared" "cd /home/iba/teaching/CS116ai/spring16/gamesearch/jwadap3/; java Gomoku" 0 0 0)
   ;(player (void) (void) "kyle" "cd /home/iba/teaching/CS116ai/spring16/gamesearch/kyle/; java GomokuClient" 0 0 0)
   ;(player (void) (void) "natalie" "cd /home/iba/teaching/CS116ai/spring16/gamesearch/nsteepletonP3/src/; java GomokuSearch" 0 0 0)
   ;(player (void) (void) "sam" "cd /home/iba/teaching/CS116ai/spring16/gamesearch/sbentz/; java GomokuClient" 0 0 0)
   ;(player (void) (void) "dray" "racket /home/wfi/CS116ai/fall13/Gomoku/drayP3/dray.rkt" 0 0 0) 
   ;(player (void) (void) "grady" "cd /home/wfi/CS116ai/fall13/Gomoku/ggoffP3; java GomokuPlayer" 0 0 0)
   ;(player (void) (void) "echoe" "cd /home/wfi/CS116ai/fall13/Gomoku/ecjonesP3/build/classes; java ecjonesp3.GomokuAgent" 0 0 0)
   ;(player (void) (void) "tim" "cd /home/wfi/CS116ai/fall13/Gomoku/tswanson; java Agent" 0 0 0)
   ;(player (void) (void) "adam" "python /home/wfi/CS116ai/fall13/Gomoku/ahessP3/client.py" 0 0 0)
   ;(player (void) (void) "nolan" "racket /home/wfi/CS116ai/fall13/Gomoku/nblew/GomokuClient.exe" 0 0 0) 
   ;(player (void) (void) "lewis" "/home/wfi/CS116ai/fall13/Gomoku/lwallP3/gomoku" 0 0 0)
   ;(player (void) (void) "newtrained" "racket /home/iba/teaching/CS150ml/Gomoku/AdaptiveGomoku2.rkt" 0 0 0)
   ;(player (void) (void) "oldtrained" "racket /home/iba/teaching/CS150ml/Gomoku/AdaptiveGomokuOriginal.rkt" 0 0 0)
   ;(player (void) (void) "random" "racket GomokuClient.rkt" 0 0 0)  
   ;
   #|(player "austin" "cd gomokutourney2/austin/GomokuProject/build/classes; java GomokuNaive" 0 (void))
   (player "chris" "cd gomokutourney2/cbetsill/bin; java gomoku" 0 (void))
   (player "connor" "cd gomokutourney2/connor; /usr/bin/python Q_Learning_Gomoku2.py" 0 (void))
   ;(player "daniel" "cd gomokutourney2/daniel/bin; java Client" 0 (void))
   (player "dillon" "cd gomokutourney2/dillon; java GomokuNN" 0 (void))
   (player "grady" "cd gomokutourney2/grady; java GomokuClient GameStates.tmp" 0 (void))
   (player "robert" "cd gomokutourney2/robert; java LearningAgent" 0 (void))
   (player "sam" "cd gomokutourney2/sam; racket Client1.rkt" 0 (void))
   (player "stefan" "cd gomokutourney2/stefan; java Gomoku localhost 17033" 0 (void)) 
   (player "tim" "cd gomokutourney2/tim; java Agent" 0 (void))
   (player "andrey" "cd gomokutourney2/andrey; java gomoku" 0 (void))
   |#
   ))

;#|
(define my-listener (get-a-listener))
(do ([ps players (cdr ps)])
  ((= (length ps) 1))
  (for ([p2 (cdr ps)])
    (let ([p1 (car ps)])
      (tournament-games 5 p1 p2 my-listener))))
;|#
