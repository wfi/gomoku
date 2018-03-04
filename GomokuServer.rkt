#lang racket

(require lang/posn htdp/draw)
(require "GomokuUtil.rkt")

(provide (all-defined-out))

(define GOMOKUPORT 17033)

;;---------------------------------------------------------------------------
;;    GOMOKU GAME SERVER
;;---------------------------------------------------------------------------

;;-------------------------------------------------------------------------------------------
;; protocol: 

;; 1. client opens socket on GOMOKUPORT to connect to server

;; 2. server (after getting two player connections) sends three groups of data:
;;  a. game-status as one of 'CONTINUING, 'WIN, 'LOSE, 'DRAW, 'FORFEIT-TIME, 'FORFEIT-MOVE
;;  b. series of lines of characters representing each row of the current SQUARE-board; characters one of: "x", "o", or " " (space)
;;  c. previous move: "r c" or "-1 -1" when very first move
;;  d. color to play: either "x" or "o"

;; 3. client sends a move:
;;  a. a single line consisting of space separated row and column values
;;-------------------------------------------------------------------------------------------

;; game constants
(define BOARD-SIZE 9)
(define MAX-MOVE-TIME 20.0)
;; graphics constants
(define CELL-SIZE 40)
(define STONE-RADIUS (floor (* CELL-SIZE 9/20)))
(define MARGIN (* CELL-SIZE 3/4))
(define WID/HEIGHT (+ (* 2 MARGIN) (* (sub1 BOARD-SIZE) CELL-SIZE)))
(start WID/HEIGHT WID/HEIGHT)
;; misc
(define START-GAME (build-vector BOARD-SIZE (lambda (r) (build-vector BOARD-SIZE (lambda (c) 'b)))))


;; a Game-State (GS) is a (vectorof (vectorof symbol))

;; a move-pair is a (cons r c) where r and c are ints for row and col of the move

;; a move is a (cons GS mp), where GS is a Game-State and mp is a move-pair

;;---------------------- UTILITIES 
;; see also GomokuUtil.rkt

;; reset-start-game : ->
(define (reset-start-game)
  (set! START-GAME (build-vector BOARD-SIZE (lambda (r) (build-vector BOARD-SIZE (lambda (c) 'b))))))


;;--------------------- DRAWING UTILITIES

;; stop the graphics engine
(define (stop-draw) (stop))

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

;; draw-win: (listof (N . N)) -> true
;; draw a red circle around the winning run
(define (draw-win wr)
  (printf "Attempting to draw winning run at stones: ~a~%" wr)
  (for* ([rc wr]
         [i (in-range 1 4)])
    (draw-circle (make-posn (+ (* (cdr rc) CELL-SIZE) MARGIN) (+ (* (car rc) CELL-SIZE) MARGIN))
                 (+ STONE-RADIUS i) 'red)
    ))

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

;;--------------------- SCORING 

;; update-score: player symbol -> void
(define (update-score p result)
  (case result
    [(WIN) (set-player-wins! p (add1 (player-wins p)))]
    [(LOSE) (set-player-losses! p (add1 (player-losses p)))]
    [(DRAW) (set-player-draws! p (add1 (player-draws p)))]
    [else (error 'update-score "unrecognized game result -- should be one of win, lose, draw")]))


;;------------------------------------------------------------------------------------------------------
;;----------------- Server Portion ---------------------------------------------------------------------

(struct player (iprt oprt name command wins draws losses proclist) #:mutable)
;; a player is a structure: (player i o n c w d l pl)
;; where i is an input-port and o is an output-port, n is a string name for the player,
;; c is the command-line to run the player controller,
;; w, d and l are numbers representing the wins, draws, and losses of the player, respectively,
;; and pl is a list of five items returned by the process function


(define (get-a-listener) (tcp-listen GOMOKUPORT))

;; read-move: intput-port -> (cons N N)
;; read a player's move from the player's input-port, returning as a dotted pair
(define (read-move iprt)
  (cons (read iprt) (read iprt)))
;; read-move-from-string: string -> (cons N N)
;; like read-move but reads the move from a single string that was read as a line
(define (read-move-from-string s)
  (with-input-from-string s
                          (lambda () (cons (read) (read)))))

;; send-game-info: string GS move-pair symbol output-port -> void
;; send the given game information to the given output-port
(define (send-game-info status gs last-move to-play oprt)
  (fprintf oprt "~a~%" status)
  (for ([row gs])
    (for ([col row]) 
      ;;(printf "~a" (if (symbol=? col 'b) #\space col))
      (fprintf oprt "~a" (if (symbol=? col 'b) #\space col)))
    ;;(printf "~%")
    (fprintf oprt "~%"))
  ;;(printf "~a~%" to-play)
  (fprintf oprt "~a ~a~%" (car last-move) (cdr last-move))
  (fprintf oprt "~a~%" to-play)
  (flush-output oprt))

;; srv-game: GS player player move-pair symbol -> (cons player player)
;; For use in tournament mode.
;; given a game-state, two players, the previous move, and a symbol for which color is to play
;; (w/ implicit understanding that p1 plays next with that color)
;; continue serving the game until completed.
;; finally return the pair of player structs with their score counts updated
(define (srv-game gs p1 p2 last-move to-play)
  (draw-game gs)
  (cond [(game-over? gs) ; terminate with actual outcome
         (let ([p1-result (win/lose/draw gs to-play)]
               [p2-result (win/lose/draw gs (toggle to-play))])
           (cond [(symbol=? p1-result 'WIN) (draw-win (get-winning-run gs to-play))]
                 [(symbol=? p2-result 'WIN) (draw-win (get-winning-run gs (toggle to-play)))])
           (printf "Result: player ~a ~a, player ~a ~a~%" (player-name p1) p1-result (player-name p2) p2-result)
           (send-game-info p1-result gs last-move to-play (player-oprt p1)) (flush-output (player-oprt p1))
           (update-score p1 p1-result)
           (send-game-info p2-result gs last-move to-play (player-oprt p2)) (flush-output (player-oprt p2))
           (update-score p2 p2-result)
           (cons p1 p2))]
        [else ; send game-status, board-state, and player-to-play to p1
         (send-game-info 'CONTINUING gs last-move to-play (player-oprt p1)) (flush-output (player-oprt p1))
         (let ([maybe-move (sync/timeout MAX-MOVE-TIME (read-line-evt (player-iprt p1)))])
           (cond [(boolean? maybe-move) ; move was NOT made in time -- forfeit-time
                  (printf "Player ~a forfeit-time, player ~a wins~%" (player-name p1) (player-name p2))
                  (send-game-info 'FORFEIT-TIME gs last-move to-play (player-oprt p1)) (update-score p1 'lose)
                  ;#|
                  (when (and (cons? (player-proclist p1))
                             (symbol=? ((fifth (player-proclist p1)) 'status) 'running))
                    ((fifth (player-proclist p1)) 'kill))
                  ;|#
                  (send-game-info 'WIN gs last-move (toggle to-play) (player-oprt p2)) (update-score p2 'win)
                  (cons p1 p2)]
                 [else ; check if valid move (i.e., on the board and vacant)
                  (let ([a-move (read-move-from-string maybe-move)]) ; get p1's move
                    (cond [(not (and (number? (car a-move)) (< -1 (car a-move) BOARD-SIZE)
                                     (number? (cdr a-move)) (< -1 (cdr a-move) BOARD-SIZE)
                                     (symbol=? 'b (vgame-spot gs (car a-move) (cdr a-move))))) ;; invalid move -- forfeit-move
                           (printf  "forfeit-move: ~a attempted move at (~a,~a), player ~a wins~%"
                                    (player-name p1) (car a-move) (cdr a-move) (player-name p2))
                           (send-game-info 'FORFEIT-move gs a-move to-play (player-oprt p1)) (update-score p1 'lose)
                           (send-game-info 'WIN gs a-move (toggle to-play) (player-oprt p2)) (update-score p2 'win)
                           (cons p1 p2)]
                          [else ;; move is valid
                           (place-move! gs a-move to-play) ; and update game-state accordingly
                           ;; call srv-game with new game-state and p2 p1 swapped
                           (srv-game gs p2 p1 a-move (toggle to-play))]))]))]         
        ))
  
;; start-a-game: tcp-listener -> ...
;; for use in head-to-head testing rather than tournament play
(define (start-a-game my-listener)
  (reset-start-game)
  (let*-values ([(ignore1) (printf "waiting for player 1 to connect~%")]
                [(p1-iprt p1-oprt) (tcp-accept my-listener)]
                [(ignore2) (printf "player 1 connected -- waiting for player 2 to connect~%")]
                [(p2-iprt p2-oprt) (tcp-accept my-listener)]
                [(ignore3) (printf "player 2 connected -- ready to play game~%")]
                [(result) (srv-game START-GAME
                                    (player p1-iprt p1-oprt "p1-black" "ad-hoc" 0 0 0 empty)
                                    (player p2-iprt p2-oprt "p2-white" "ad-hoc" 0 0 0 empty)
				    (cons -1 -1)
                                    'x)])
    (close-output-port p1-oprt)
    (close-input-port p1-iprt)
    (close-output-port p2-oprt)
    (close-input-port p2-iprt)
    (printf "finished game with ~a win/lose/draw ~a/~a/~a and ~a win/lose/draw ~a/~a/~a~%"
            (player-name (car result))
            (player-wins (car result)) (player-losses (car result)) (player-draws (car result))
            (player-name (cdr result))
            (player-wins (cdr result)) (player-losses (cdr result)) (player-draws (cdr result)))
    (tcp-close my-listener)
    ;(start-a-game my-listener)
    ))

;; the next two line should be commented for tournament play
(define my-listener (get-a-listener))
(start-a-game my-listener)
