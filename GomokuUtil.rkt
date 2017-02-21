#lang racket

(provide board-height
         board-width
         game-over?
         game-result
         win/lose/draw
         vgame-spot
         place-move
         place-move!
         toggle
         (struct-out espot)
         
         )

(define IN-ROW-TO-WIN 5)

;; a Game-State (GS) is a (vectorof (vectorof symbol))

;; a move-pair is a (cons r c) where r and c are ints for row and col of the move

;; a move is a (cons GS mp), where GS is a Game-State and mp is a move-pair


;; board-height : GS -> number
(define (board-height gs) (vector-length gs))
;; board-width : GS -> number
(define (board-width gs) (vector-length (vector-ref gs 0)))

;;---------------------- EVALUATE GAME STATUS

;; no-valid-moves?: GS -> boolean
;; determine if the board is complete covered
(define (no-valid-moves? gs)
  (not (for*/or ([r (board-height gs)]
                 [c (board-width gs)])
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

;;---------------------- MANIPULATE BOARD GAME-STATES

;; vgame-spot: GS N N -> symbol
;; the vector version
(define (vgame-spot gs r c) (vector-ref (vector-ref gs r) c))

;; place-move: GS (N . N) symbol -> GS
;; copy the given game-state and then place the given to-play player's move.
;; ASSUME the move is valid!
(define (place-move gs move-pair to-play)
  (let ([new-gs (build-vector (board-height gs) (lambda (r) (vector-copy (vector-ref gs r))))])
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

;;-----------------------------------------------------------
;; HELPER CODE FOR CHECKING N-IN-A-ROW

(define-struct espot (l ul u ur))
;; an espot is a structure: (make-espot n1 n2 n3 n4)
;; where n1 through n4 are numbers
;; the numbers represent the length of the line of stones in the respective directions

;; make-inline-grid: GS sybol -> (vectorof (vectorof espot))
;; create a grid of espots that describe to-plays (tp) in a line
(define (make-inline-grid gs tp)
  (local ((define esgrid (build-vector (board-height gs) (lambda (_) (build-vector (board-width gs) (lambda (_) 'dummy)))))
          (define (vbuild-espot r c)
            (make-espot (if (zero? c) 1 (add1 (espot-l (vector-ref (vector-ref esgrid r) (sub1 c)))))
                        (if (or (zero? c) (zero? r)) 1 (add1 (espot-ul (vector-ref (vector-ref esgrid (sub1 r)) (sub1 c)))))
                        (if (zero? r) 1 (add1 (espot-u (vector-ref (vector-ref esgrid (sub1 r)) c))))
                        (if (or (= c (sub1 (board-height gs))) (zero? r)) 1 (add1 (espot-ur (vector-ref (vector-ref esgrid (sub1 r)) (add1 c)))))))
          (define (vprow! r)
            (local ((define the-row (vector-ref esgrid r)))
              (for ([i (board-width gs)])
                (vector-set! the-row i 
                             (if (symbol=? (vgame-spot gs r i) tp)
                                 (vbuild-espot r i)
                                 (make-espot 0 0 0 0)))))))
    (begin (for ([r (board-height gs)]) (vprow! r))
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
