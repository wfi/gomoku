#lang racket

(require "GomokuServer.rkt")


(define-struct pthing (name command score player) #:mutable)
;; a pthing is a structure: (make-pthing n c s p)
;; where n is a string, c is a string and s is a int, and p is a player (containing an input and output ports)


;; reset-pthing: pthing -> void
(define (reset-pthing p)
  (set-pthing-score! p 0))

;; nu-srv-game: GS pthing pthing symbol -> ...
(define (nu-srv-game gs pt1 pt2 to-play)
  (draw-game gs)
  ;; unless game-over, do:
  (let ([p1 (pthing-player pt1)]
        [p2 (pthing-player pt2)])
    (cond [(not (game-end? gs))
           ;; send game-status, board-state, and player-to-play to p1
           (send-game-info 'continuing gs to-play (player-oprt p1)) (flush-output (player-oprt p1))
           (let* ([start-time (current-milliseconds)]
                  [a-move (read-move (player-iprt p1))]) ; read move from p1
             ;; check if 
             (cond [(> (- (current-milliseconds) start-time) 2000)
                    (send-game-info 'forfeit-time gs to-play (player-oprt p1))
                    (send-game-info 'win gs (toggle to-play) (player-oprt p2))
                    ;(printf "nu-srv-game: adding 1 to score for ~a will have ~a~%" (pthing-name pt2) (pthing-score pt2))
                    (set-pthing-score! pt2 (add1 (pthing-score pt2)))]
                   [(not (and (number? (car a-move)) (< -1 (car a-move) BOARD-SIZE)
                              (number? (cdr a-move)) (< -1 (cdr a-move) BOARD-SIZE)
                              (symbol=? 'b (vgame-spot gs (car a-move) (cdr a-move)))))
                    (send-game-info 'forfeit-move gs to-play (player-oprt p1))
                    (send-game-info 'win gs (toggle to-play) (player-oprt p2))
                    (set-pthing-score! pt2 (add1 (pthing-score pt2)))]
                   [else ;; move is valid (i.e., on the board and vacant)
                    (place-move! gs a-move to-play) ; and update game-state accordingly
                    ;; call srv-game with new game-state and p2 p1 swapped
                    (nu-srv-game gs pt2 pt1 (toggle to-play))]))]
          [else ;; terminate with actual outcome
           ;(printf "nu-srv-game: in final else with outcome for ~a given by win/lose/draw as: ~a~%" to-play (win/lose/draw gs to-play))
           (send-game-info (win/lose/draw gs to-play) gs to-play (player-oprt p1)) (flush-output (player-oprt p1))
           (send-game-info (win/lose/draw gs (toggle to-play)) gs to-play (player-oprt p2)) (flush-output (player-oprt p2))
           (case (win/lose/draw gs to-play)
             [(win) (set-pthing-score! p1 (add1 (pthing-score p1)))]
             [(lose forfeit-move forfeit-time) (set-pthing-score! pt2 (add1 (pthing-score pt2)))]
             [else (void)])
           ])))


;; tournament-games: int pthing pthing tcp-listener -> ...
;; run num-games games between the two players with given names, finally reporting the results
(define (tournament-games num-games p1 p2 my-listener)
  (cond [(or (> (pthing-score p1) (floor (/ num-games 2)))
             (> (pthing-score p2) (floor (/ num-games 2))))
         (printf "~%Tournament Pairing Result~%Player: ~a won ~a~%Player: ~a won ~a~%~%" (pthing-name p1) (pthing-score p1) (pthing-name p2) (pthing-score p2))
         (reset-pthing p1) (reset-pthing p2)
         ]
        [else 
         (printf "Let's play a game~%")
         (let*-values ([(start-p1) (process (pthing-command p1))]
                       [(p1-iprt p1-oprt) (tcp-accept my-listener)]
                       [(start-p2) (process (pthing-command p2))]
                       [(p2-iprt p2-oprt) (tcp-accept my-listener)]
                       )
           (reset-start-game)
           (set-pthing-player! p1 (make-player p1-iprt p1-oprt))
           (set-pthing-player! p2 (make-player p2-iprt p2-oprt))
           (nu-srv-game START-GAME p1 p2 'x)
           (close-output-port p1-oprt)(close-input-port p1-iprt)
           (close-output-port p2-oprt)(close-input-port p2-iprt)
           (printf "~a's standard out~%" (pthing-name p1))
           (for ([l (in-port read-line (first start-p1))]) (displayln l))
           (printf "~a's standard error~%" (pthing-name p1))
           (for ([l (in-port read-line (fourth start-p1))]) (displayln l))
           (printf "~%")
           (printf "~a's standard out~%" (pthing-name p2))
           (for ([l (in-port read-line (first start-p2))]) (displayln l))
           (printf "~a's standard error~%" (pthing-name p2))
           (for ([l (in-port read-line (fourth start-p2))]) (displayln l))
           (printf "~%")
           (printf "~%")
           ((fifth start-p1) 'kill)
           ((fifth start-p2) 'kill)
           ;(tcp-close my-listener)
           (tournament-games num-games p2 p1 my-listener))]))
      

(define players
  (list
   (make-pthing "chris" "cd /home/iba/teaching/CS116ai/spring16/gamesearch/cbetsillP03/; java gomoku" 0 (void))
   (make-pthing "hunter" "cd /home/iba/teaching/CS116ai/spring16/gamesearch/hmcgushionP3/; java Controller3AlphaBeta04" 0 (void))
   (make-pthing "random" "racket RandomPlayer.rkt" 0 (void))
   ;(make-pthing "james" "java /home/iba/teaching/CS116ai/spring16/gamesearch/jbyronP3/GomokuAgent" 0 (void))
   ;(make-pthing "jacob" "cd /home/iba/teaching/CS116ai/spring16/gamesearch/jochsP3/; java GomokuSearch" 0 (void))
   ;(make-pthing "jared" "java /home/iba/teaching/CS116ai/spring16/gamesearch/jwadap3/Gomoku" 0 (void))
   ;(make-pthing "kyle" "java /home/iba/teaching/CS116ai/spring16/gamesearch/kyle/GomokuClient" 0 (void))
   ;(make-pthing "natalie" "java /home/iba/teaching/CS116ai/spring16/gamesearch/nsteepletonP3/GomokuSearch" 0 (void))
   ;(make-pthing "sam" "java /home/iba/teaching/CS116ai/spring16/gamesearch/sbentz/GomokuClient" 0 (void))
   ;'(make-pthing "dray" "racket /home/wfi/CS116ai/fall13/Gomoku/drayP3/dray.rkt" 0 (void)) 
   ;'(make-pthing "grady" "cd /home/wfi/CS116ai/fall13/Gomoku/ggoffP3; java GomokuPlayer" 0 (void))
   ;'(make-pthing "echoe" "cd /home/wfi/CS116ai/fall13/Gomoku/ecjonesP3/build/classes; java ecjonesp3.GomokuAgent" 0 (void))
   ;'(make-pthing "tim" "cd /home/wfi/CS116ai/fall13/Gomoku/tswanson; java Agent" 0 (void))
   ;'(make-pthing "adam" "python /home/wfi/CS116ai/fall13/Gomoku/ahessP3/client.py" 0 (void))
   ;'(make-pthing "nolan" "racket /home/wfi/CS116ai/fall13/Gomoku/nblew/GomokuClient.exe" 0 (void)) 
   ;'(make-pthing "lewis" "/home/wfi/CS116ai/fall13/Gomoku/lwallP3/gomoku" 0 (void))
   ;(make-pthing "newtrained" "racket /home/iba/teaching/CS150ml/Gomoku/AdaptiveGomoku2.rkt" 0 (void))
   ;(make-pthing "oldtrained" "racket /home/iba/teaching/CS150ml/Gomoku/AdaptiveGomokuOriginal.rkt" 0 (void))
   ;(make-pthing "random" "racket GomokuClient.rkt" 0 (void))  
   ;
   #|(make-pthing "austin" "cd gomokutourney2/austin/GomokuProject/build/classes; java GomokuNaive" 0 (void))
   (make-pthing "chris" "cd gomokutourney2/cbetsill/bin; java gomoku" 0 (void))
   (make-pthing "connor" "cd gomokutourney2/connor; /usr/bin/python Q_Learning_Gomoku2.py" 0 (void))
   ;(make-pthing "daniel" "cd gomokutourney2/daniel/bin; java Client" 0 (void))
   (make-pthing "dillon" "cd gomokutourney2/dillon; java GomokuNN" 0 (void))
   (make-pthing "grady" "cd gomokutourney2/grady; java GomokuClient GameStates.tmp" 0 (void))
   (make-pthing "robert" "cd gomokutourney2/robert; java LearningAgent" 0 (void))
   (make-pthing "sam" "cd gomokutourney2/sam; racket Client1.rkt" 0 (void))
   (make-pthing "stefan" "cd gomokutourney2/stefan; java Gomoku localhost 17033" 0 (void)) 
   (make-pthing "tim" "cd gomokutourney2/tim; java Agent" 0 (void))
   (make-pthing "andrey" "cd gomokutourney2/andrey; java gomoku" 0 (void))
   |#
   ))

;#|
(define my-listener (get-a-listener))
(do ([ps players (cdr ps)])
  ((= (length ps) 1))
  (for ([p2 (cdr ps)])
    (let ([p1 (car ps)])
      (tournament-games 3 p1 p2 my-listener))))
;|#

#|
(tournament-games 7
                  (make-pthing "newtrained" "racket /home/iba/teaching/CS150ml/Gomoku/AdaptiveGomoku2.rkt" 0 (void))
                  (make-pthing "oldtrained" "racket /home/iba/teaching/CS150ml/Gomoku/AdaptiveGomokuOriginal.rkt" 0 (void))
                  ;(make-pthing "random2" "racket GomokuClient.rkt" 0 (void))
                  ;(make-pthing "dray" "racket /home/wfi/CS116ai/fall13/Gomoku/drayP3/dray.rkt" 0 (void)) 
                  ;(make-pthing "grady" "cd /home/wfi/CS116ai/fall13/Gomoku/ggoffP3; java GomokuPlayer" 0 (void))
                  ;(make-pthing "echoe" "cd /home/wfi/CS116ai/fall13/Gomoku/ecjonesP3/build/classes; java ecjonesp3.GomokuAgent" 0 (void))
                  ;(make-pthing "tim" "cd /home/wfi/CS116ai/fall13/Gomoku/tswanson; java Agent" 0 (void))
                  ;(make-pthing "adam" "python /home/wfi/CS116ai/fall13/Gomoku/ahessP3/client.py" 0 (void))
                  ;(make-pthing "nolan" "/home/wfi/CS116ai/fall13/Gomoku/nblew/GomokuClient.exe" 0 (void)) 
                  ;(make-pthing "lewis" "/home/wfi/CS116ai/fall13/Gomoku/lwallP3/gomoku" 0 (void))
                  ;(make-pthing "connor" "cd /home/wfi/CS116ai/fall13/Gomoku/crivaP3; java GomokuClient localhost" 0 (void))
                  my-listener)
|#