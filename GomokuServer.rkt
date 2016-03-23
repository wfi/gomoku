#lang racket

(require lang/posn htdp/draw)

(provide (all-defined-out))

(define GOMOKUPORT 17033)

;;---------------------------------------------------------------------------
;;    GOMOKU GAME SERVER
;;---------------------------------------------------------------------------

;;-------------------------------------------------------------------------------------------
;; protocol: 

;; 1. client opens socket on GOMOKUPORT to connect to server

;; 2. server (after getting two player connections) sends three groups of data:
;;  a. game-status as one of 'continuing, 'win, 'lose, 'draw, 'forfeit-time, 'forfeit-move
;;  b. series of lines of characters representing each row of the current SQUARE-board; characters one of: "x", "o", or " " (space)
;;  c. color to play: either "x" or "o"

;; 3. client sends a move:
;;  a. a single line consisting of space separated row and column values
;;-------------------------------------------------------------------------------------------


#|
Issues: add thread for reading move from client so that can interrupt if goes over time.
|#


(define IN-ROW-TO-WIN 5)
(define BOARD-SIZE 5)
(define CELL-SIZE 40)
(define STONE-RADIUS (floor (* CELL-SIZE 9/20)))
(define MARGIN (* CELL-SIZE 3/4))
(define START-GAME (build-vector BOARD-SIZE (lambda (r) (build-vector BOARD-SIZE (lambda (c) 'b)))))
(define WID/HEIGHT (+ (* 2 MARGIN) (* (sub1 BOARD-SIZE) CELL-SIZE)))

(start WID/HEIGHT WID/HEIGHT)

;; a Game-State (GS) is a (vectorof (vectorof symbol))

;; a move-pair is a (cons r c) where r and c are ints for row and col of the move

;; a move is a (cons GS mp), where GS is a Game-State and mp is a move-pair

;;---------------------- UTILITIES

;; reset-start-game : ->
(define (reset-start-game)
  (set! START-GAME (build-vector BOARD-SIZE (lambda (r) (build-vector BOARD-SIZE (lambda (c) 'b))))))

;; vgame-spot: GS N N -> symbol
;; the vector version
(define (vgame-spot gs r c) (vector-ref (vector-ref gs r) c))

;; place-move: GS (N . N) symbol -> GS
;; copy the given game-state and then place the given to-play player's move.
;; ASSUME the move is valid!
(define (place-move gs move-pair to-play)
  (let ([new-gs (build-vector BOARD-SIZE (lambda (r) (vector-copy (vector-ref gs r))))])
    (vector-set! (vector-ref new-gs (car move-pair)) (cdr move-pair) to-play)
    new-gs))

;; place-move! : GS (N . N) symbol -> (void)
(define (place-move! gs move-pair to-play)
  (vector-set! (vector-ref gs (car move-pair)) (cdr move-pair) to-play))

;; toggle: symbol -> symbol
;; flips from one player to the other
(define (toggle tp)
  (case tp
    [(x) 'o]
    [(o) 'x]
    [else (error 'toggle (format "invalid player to-play: ~a" tp))]))


;;--------------------- DRAWING UTILITIES

;; draw-grid: GS -> true
;; ASSUME square board
(define (draw-grid ignore-state)
  (local 
    ((define (dg-aux n)
       (cond [(zero? n) true]
             [else (draw-solid-line (make-posn (+ (* (sub1 n) CELL-SIZE) MARGIN) MARGIN) 
                                         (make-posn (+ (* (sub1 n) CELL-SIZE) MARGIN) 
                                                    (- WID/HEIGHT MARGIN))
                                         'black)
                   (draw-solid-line (make-posn MARGIN (+ (* (sub1 n) CELL-SIZE) MARGIN))
                                    (make-posn (- WID/HEIGHT MARGIN) 
                                               (+ (* (sub1 n) CELL-SIZE) MARGIN))
                                    'black)
                   (dg-aux (sub1 n))])))
    (dg-aux BOARD-SIZE)))

;; draw-x: number number -> true
;; draws a black stone on the zero-based LOGICAL INTERSECTION of the board
(define (draw-x r c)
  (local (;; draw-x-help: number number -> true
	  ;; draws a black circle at the physical SCREEN-BASED pixel coordinates specified
	  (define (draw-x-help y x)
	    (draw-solid-disk (make-posn x y) STONE-RADIUS 'black)))
    (draw-x-help (+ (* r CELL-SIZE) MARGIN) (+ (* c CELL-SIZE) MARGIN))))

;; draw-o: number number -> true
;; draws an white stone in the zero-based LOGICAL INTERSECTION of the board
(define (draw-o r c)
  (local (;; draw-o-help: number number -> true
	  ;; draws a circle at the physical SCREEN-BASED pixel coordinates specified
	  (define (draw-o-help y x)
            (draw-solid-disk (make-posn x y) STONE-RADIUS 'white)
            (draw-circle (make-posn x y) STONE-RADIUS 'black)))
    (draw-o-help (+ (* r CELL-SIZE) MARGIN) (+ (* c CELL-SIZE) MARGIN))))

;; draw-game: GS -> true
;; draw the gamestate
(define (draw-game gs)
  (clear-solid-rect (make-posn 0 0) WID/HEIGHT WID/HEIGHT)
  (draw-grid gs)
  (do ([row 0 (add1 row)])
    ((= row BOARD-SIZE) true)
    (do ([col 0 (add1 col)])
      ((= col BOARD-SIZE))
      (cond [(symbol=? (vgame-spot gs row col) 'x)
             (draw-x row col)]
            [(symbol=? (vgame-spot gs row col) 'o)
             (draw-o row col)]))))


;;---------------------- EVALUATE GAME STATUS

;; no-valid-moves?: GS -> boolean
;; determine if the board is complete covered
(define (no-valid-moves? gs)
  (not (for*/or ([r BOARD-SIZE]
                 [c BOARD-SIZE])
         (symbol=? 'b (vgame-spot gs r c)))))

;; game-end?: GS -> boolean
;; determine if the game is over
(define (game-end? gs)
  (not (symbol=? (game-result gs) '?)))

;; game-result: GS -> symbol['x,'o,'d,'?]
;; determine if the game is a win for either 'x or 'o, a draw 'd, or not finished '?
(define (game-result gs)
  (cond [(n-in-row? gs 'x) 'x]
        [(n-in-row? gs 'o) 'o]
        [(no-valid-moves? gs) 'd]
        [else '?]))

(define (win/lose/draw gs to-play)
  (let ([res (game-result gs)])
    (case res
      [(x) (if (symbol=? 'x to-play) 'win 'lose)]
      [(o) (if (symbol=? 'o to-play) 'win 'lose)]
      [(d) 'draw]
      [else (error 'win/lose/draw "game not finished")])))

;;-----------------------------------------------------------
;; HELPER CODE FOR CHECKING N-IN-A-ROW

(define-struct espot (l ul u ur))
;; an espot is a structure: (make-espot n1 n2 n3 n4)
;; where n1 through n4 are numbers
;; the numbers represent the length of the line of stones in the respective directions

;; make-inline-grid: GS sybol -> (vectorof (vectorof espot))
;; create a grid of espots that describe to-plays (tp) in a line
(define (make-inline-grid gs tp)
  (local ((define esgrid (build-vector BOARD-SIZE (lambda (_) (build-vector BOARD-SIZE (lambda (_) 'dummy)))))
          (define (vbuild-espot r c)
            (make-espot (if (zero? c) 1 (add1 (espot-l (vector-ref (vector-ref esgrid r) (sub1 c)))))
                        (if (or (zero? c) (zero? r)) 1 (add1 (espot-ul (vector-ref (vector-ref esgrid (sub1 r)) (sub1 c)))))
                        (if (zero? r) 1 (add1 (espot-u (vector-ref (vector-ref esgrid (sub1 r)) c))))
                        (if (or (= c (sub1 BOARD-SIZE)) (zero? r)) 1 (add1 (espot-ur (vector-ref (vector-ref esgrid (sub1 r)) (add1 c)))))))
          (define (vprow! r)
            (local ((define the-row (vector-ref esgrid r)))
              (for ([i BOARD-SIZE])
                (vector-set! the-row i 
                             (if (symbol=? (vgame-spot gs r i) tp)
                                 (vbuild-espot r i)
                                 (make-espot 0 0 0 0)))))))
    (begin (for ([r BOARD-SIZE]) (vprow! r))
           esgrid)))

;; n-in-row?: GS symbol -> boolean
;; determine if there are IN-ROW-TO-WIN stones of the player p in a line of the given game-state
(define (n-in-row? gs p)
  (local ((define ig (make-inline-grid gs p)))
    (positive? (vector-count 
                (lambda (row)
                  (positive? (vector-count
                              (lambda (e) (or (= IN-ROW-TO-WIN (espot-l e))
                                              (= IN-ROW-TO-WIN (espot-ul e))
                                              (= IN-ROW-TO-WIN (espot-u e))
                                              (= IN-ROW-TO-WIN (espot-ur e))))
                              row)))
                ig))))


;;------------------------------------------------------------------------------------------------------
;;----------------- Server Portion ---------------------------------------------------------------------

(define-struct player (iprt oprt))
;; a player is a structure: (make-player i o) where i is an input-port and o is an output-port

(define (get-a-listener) (tcp-listen GOMOKUPORT))

;; read-move: intput-port -> (cons N N)
;; read a player's move from the player's input-port, returning as a dotted pair
(define (read-move iprt)
  (cons (read iprt) (read iprt)))

;; send-game-info: string GS symbol output-port -> void
;; send the given game information to the given output-port
(define (send-game-info status gs to-play oprt)
  (fprintf oprt "~a~%" status)
  (for ([row gs])
    (for ([col row]) 
      ;;(printf "~a" (if (symbol=? col 'b) #\space col))
      (fprintf oprt "~a" (if (symbol=? col 'b) #\space col)))
    ;;(printf "~%")
    (fprintf oprt "~%"))
  ;;(printf "~a~%" to-play)
  (fprintf oprt "~a~%" to-play))

;; srv-game: GS player player symbol -> 
(define (srv-game gs p1 p2 to-play)
  (draw-game gs)
  ;; unless game-over, do:
  (cond [(not (game-end? gs))
         ;; send game-status, board-state, and player-to-play to p1
         (send-game-info 'continuing gs to-play (player-oprt p1)) (flush-output (player-oprt p1))
         (let* ([start-time (current-milliseconds)]
                [a-move (read-move (player-iprt p1))]) ; read move from p1
           ;; check if 
           (cond [(> (- (current-milliseconds) start-time) 2000)
                  (send-game-info 'forfeit-time gs to-play (player-oprt p1))
                  (send-game-info 'win gs (toggle to-play) (player-oprt p2))]
                 [(not (and (< -1 (car a-move) BOARD-SIZE) (< -1 (cdr a-move) BOARD-SIZE)
                            (symbol=? 'b (vgame-spot gs (car a-move) (cdr a-move)))))
                  (printf  "forfeit-move: attempted move at (~a,~a)~%" (car a-move) (cdr a-move))
                  (send-game-info 'forfeit-move gs to-play (player-oprt p1))
                  (send-game-info 'win gs (toggle to-play) (player-oprt p2))]
                 [else ;; move is valid (i.e., on the board and vacant)
                  (place-move! gs a-move to-play) ; and update game-state accordingly
                  ;; call srv-game with new game-state and p2 p1 swapped
                  (srv-game gs p2 p1 (toggle to-play))]))]
        [else ;; terminate with actual outcome
         (send-game-info (win/lose/draw gs to-play) gs to-play (player-oprt p1)) (flush-output (player-oprt p1))
         (send-game-info (win/lose/draw gs (toggle to-play)) gs to-play (player-oprt p2)) (flush-output (player-oprt p2))
         ]))
  
;; serve-a-game: tcp-listener -> ...
(define (serve-a-game my-listener)
  (let*-values ([(p1-iprt p1-oprt) (tcp-accept my-listener)]
                [(p2-iprt p2-oprt) (tcp-accept my-listener)])
    (printf "accepted two connections!~n")
    (reset-start-game)
    (srv-game START-GAME (make-player p1-iprt p1-oprt) (make-player p2-iprt p2-oprt) 'x)
    (close-output-port p1-oprt)
    (close-input-port p1-iprt)
    (close-output-port p2-oprt)
    (close-input-port p2-iprt)
    (printf "finished game~%")
    ;(tcp-close my-listener)
    (serve-a-game my-listener)
    ))

(define my-listener (get-a-listener))
(serve-a-game my-listener)

;;---------------------------------------------------------------------------------

(define-struct pthing (name command score player) #:mutable)
;; a pthing is a structure: (make-pthing n c s p)
;; where n is a string, c is a string and s is a int, and p is a player (containing an input and output ports)


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
;; run three games between the two players with given names, finally reporting the results
(define (tournament-games num-games p1 p2 my-listener)
  (cond [(or (> (pthing-score p1) (floor (/ num-games 2)))
             (> (pthing-score p2) (floor (/ num-games 2))))
         (printf "Player: ~a won ~a~%Player: ~a won ~a~%" (pthing-name p1) (pthing-score p1) (pthing-name p2) (pthing-score p2))]
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
           (close-output-port p1-oprt)
           (close-input-port p1-iprt)
           (close-output-port p2-oprt)
           (close-input-port p2-iprt)
           (printf "~a's standard out~%" (pthing-name p1))
           (for ([l (in-port read-line (first start-p1))]) (displayln l))
           (printf "~a's standard error~%" (pthing-name p1))
           (for ([l (in-port read-line (fourth start-p1))]) (displayln l))
           (printf "~a's standard out~%" (pthing-name p2))
           (for ([l (in-port read-line (first start-p2))]) (displayln l))
           (printf "~a's standard error~%" (pthing-name p2))
           (for ([l (in-port read-line (fourth start-p2))]) (displayln l))
           ((fifth start-p1) 'kill)
           ((fifth start-p2) 'kill)
           ;(tcp-close my-listener)
           (tournament-games num-games p2 p1 my-listener))])) 
      

#|
(make-pthing "random" "racket GomokuClient.rkt" 0 (void)) 
(make-pthing "dray" "racket /home/wfi/CS116ai/fall13/Gomoku/drayP3/dray.rkt" 0 (void)) 
|#
#|
(define players
  (list
   (make-pthing "chris" "java /home/iba/teaching/CS116ai/spring16/gamesearch/cbetsillP03/gomoku" 0 (void))
   (make-pthing "hunter" "java /home/iba/teaching/CS116ai/spring16/gamesearch/hmcgushionP3/Controller3AlphaBeta04" 0 (void))
   (make-pthing "james" "java /home/iba/teaching/CS116ai/spring16/gamesearch/jbyronP3/GomokuAgent" 0 (void))
   (make-pthing "jacob" "java /home/iba/teaching/CS116ai/spring16/gamesearch/jochsP3/GomokuSearch" 0 (void))
   (make-pthing "jared" "java /home/iba/teaching/CS116ai/spring16/gamesearch/jwadap3/Gomoku" 0 (void))
   (make-pthing "kyle" "java /home/iba/teaching/CS116ai/spring16/gamesearch/kyle/GomokuClient" 0 (void))
   (make-pthing "natalie" "java /home/iba/teaching/CS116ai/spring16/gamesearch/nsteepletonP3/GomokuSearch" 0 (void))
   (make-pthing "sam" "java /home/iba/teaching/CS116ai/spring16/gamesearch/sbentz/GomokuClient" 0 (void))
   ;'(make-pthing "dray" "racket /home/wfi/CS116ai/fall13/Gomoku/drayP3/dray.rkt" 0 (void)) 
   ;'(make-pthing "grady" "cd /home/wfi/CS116ai/fall13/Gomoku/ggoffP3; java GomokuPlayer" 0 (void))
   ;'(make-pthing "echoe" "cd /home/wfi/CS116ai/fall13/Gomoku/ecjonesP3/build/classes; java ecjonesp3.GomokuAgent" 0 (void))
   ;'(make-pthing "tim" "cd /home/wfi/CS116ai/fall13/Gomoku/tswanson; java Agent" 0 (void))
   ;'(make-pthing "adam" "python /home/wfi/CS116ai/fall13/Gomoku/ahessP3/client.py" 0 (void))
   ;'(make-pthing "nolan" "racket /home/wfi/CS116ai/fall13/Gomoku/nblew/GomokuClient.exe" 0 (void)) 
   ;'(make-pthing "lewis" "/home/wfi/CS116ai/fall13/Gomoku/lwallP3/gomoku" 0 (void))
   ))

(do ([ps players (cdr ps)])
  ((= (length ps) 1))
  (for ([p2 (cdr ps)])
    (let ([p1 (car ps)])
      (tournament-games 2 p1 p2 my-listener))))
|#
