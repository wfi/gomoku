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

;; game constants
(define BOARD-SIZE 11)
(define MAX-MOVE-TIME 2.0)
;; graphics constants
(define IN-ROW-TO-WIN 5)
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

;; game-over?: GS -> boolean
;; determine if the game is over
(define (game-over? gs)
  (not (symbol=? (game-result gs) '?)))

;; game-result: GS -> symbol['x,'o,'d,'?]
;; determine if the game is a win for either 'x or 'o, a draw 'd, or not finished '?
(define (game-result gs)
  (cond [(n-in-row? gs 'x) 'x]
        [(n-in-row? gs 'o) 'o]
        [(no-valid-moves? gs) 'd]
        [else '?]))

;; win/lose/draw: GS symbol -> (or 'win 'lose 'draw)
(define (win/lose/draw gs to-play)
  (let ([res (game-result gs)])
    (case res
      [(x) (if (symbol=? 'x to-play) 'win 'lose)]
      [(o) (if (symbol=? 'o to-play) 'win 'lose)]
      [(d) 'draw]
      [else (error 'win/lose/draw "game not finished")])))

;; update-score: player symbol -> void
(define (update-score p result)
  (case result
    [(win) (set-player-wins! p (add1 (player-wins p)))]
    [(lose) (set-player-losses! p (add1 (player-losses p)))]
    [(draw) (set-player-draws! p (add1 (player-draws p)))]
    [else (error 'update-score "unrecognized game result -- should be one of win, lose, draw")]))

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
  (fprintf oprt "~a~%" to-play)
  (flush-output oprt))

;; srv-game: GS player player symbol -> (cons player player)
;; given a game-state, two players, and a symbol for which color is to play
;; (w/ implicit understanding that p1 plays next with that color)
;; continue serving the game until completed.
;; finally return the pair of player structs with their score counts updated
(define (srv-game gs p1 p2 to-play)
  (draw-game gs)
  (cond [(game-over? gs) ; terminate with actual outcome
         (let ([p1-result (win/lose/draw gs to-play)]
               [p2-result (win/lose/draw gs (toggle to-play))])
           (send-game-info p1-result gs to-play (player-oprt p1)) (flush-output (player-oprt p1))
           (update-score p1 p1-result)
           (send-game-info p2-result gs to-play (player-oprt p2)) (flush-output (player-oprt p2))
           (update-score p2 p2-result)
           (cons p1 p2))]
        [else ; send game-status, board-state, and player-to-play to p1
         (send-game-info 'continuing gs to-play (player-oprt p1)) (flush-output (player-oprt p1))
         (let ([maybe-move (sync/timeout MAX-MOVE-TIME (read-line-evt (player-iprt p1)))])
           (cond [(boolean? maybe-move) ; move was NOT made in time -- forfeit-time
                  (send-game-info 'forfeit-time gs to-play (player-oprt p1)) (update-score p1 'lose)
                  ;#|
                  (when (and (cons? (player-proclist p1))
                             (symbol=? ((fifth (player-proclist p1)) 'status) 'running))
                    ((fifth (player-proclist p1)) 'kill))
                  ;|#
                  (send-game-info 'win gs (toggle to-play) (player-oprt p2)) (update-score p2 'win)
                  (cons p1 p2)]
                 [else ; check if valid move (i.e., on the board and vacant)
                  (let ([a-move (read-move-from-string maybe-move)]) ; get p1's move
                    (cond [(not (and (number? (car a-move)) (< -1 (car a-move) BOARD-SIZE)
                                     (number? (cdr a-move)) (< -1 (cdr a-move) BOARD-SIZE)
                                     (symbol=? 'b (vgame-spot gs (car a-move) (cdr a-move))))) ;; invalid move -- forfeit-move
                           (printf  "forfeit-move: attempted move at (~a,~a)~%" (car a-move) (cdr a-move))
                           (send-game-info 'forfeit-move gs to-play (player-oprt p1)) (update-score p1 'lose)
                           (send-game-info 'win gs (toggle to-play) (player-oprt p2)) (update-score p2 'win)
                           (cons p1 p2)]
                          [else ;; move is valid
                           (place-move! gs a-move to-play) ; and update game-state accordingly
                           ;; call srv-game with new game-state and p2 p1 swapped
                           (srv-game gs p2 p1 (toggle to-play))]))]))]         
        ))
  
;; serve-a-game: tcp-listener -> ...
(define (serve-a-game my-listener)
  (reset-start-game)
  (let*-values ([(ignore1) (printf "waiting for player 1 to connect~%")]
                [(p1-iprt p1-oprt) (tcp-accept my-listener)]
                [(ignore2) (printf "player 1 connected -- waiting for player 2 to connect~%")]
                [(p2-iprt p2-oprt) (tcp-accept my-listener)]
                [(ignore3) (printf "player 2 connected -- ready to play game~%")]
                [(result) (srv-game START-GAME
                                    (player p1-iprt p1-oprt "p1-black" "ad-hoc" 0 0 0 empty)
                                    (player p2-iprt p2-oprt "p2-white" "ad-hoc" 0 0 0 empty)
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
    ;(tcp-close my-listener)
    (serve-a-game my-listener)
    ))

; the next two line should be commented for tournament play
(define my-listener (get-a-listener))
(serve-a-game my-listener)

