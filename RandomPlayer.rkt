#lang racket


;;---------------------------------------
;;  GOMOKU CLIENT
;;---------------------------------------

(define GOMOKUPORT 17033)

;;-------------------------------------------------------------------------------------------
;; protocol: See GomokuServer.rkt for protocol
;;-------------------------------------------------------------------------------------------

;; random-move: (listof string) -> string
;; generate a valid random move on the given raw board of the given size
(define (random-move bstrings)
  (let* ([brd-sz (string-length (car bstrings))]
         [moves (for*/list ([r brd-sz]
                            [c brd-sz]
                            #:when (char=? #\space (string-ref (list-ref bstrings r) c)))
                  (cons r c))])
    (if (empty? moves)
        (error 'random-move "no moves possible -- game over")
        (let ([rmv (list-ref moves (random (length moves)))])
          (format "~a ~a" (car rmv) (cdr rmv))))
    ))

;; read-board: input-port -> (listof string)
;; read a SQUARE-board from the port, packaging it as list of rows (top to bottom) where each row is a string
(define (read-board iprt)
  (let ([first-row (read-line iprt)])
    (cons first-row
          (for/list ([r (sub1 (string-length first-row))]) ;; assume board is square
            (read-line iprt)))))

;; net-play-moves: input-port output-port -> symbol
;; plays a networked GOMOKU game using the given socket ports to read game-states and write move-pairs
;; finally returns one of 'win, 'lose, or 'draw
(define (net-play-moves iprt oprt)
  (let* (;read game-status/game-state/to-play-player triple
         [gstatus (string->symbol (read-line iprt))]
         [board-state (read-board iprt)]
         [to-play (read-line iprt)])
    ;; repeat until game over
    (cond [(symbol=? gstatus 'continuing)
           ;; display the game-state information
           #|
           (displayln gstatus)
           (for ([row board-state]) (displayln row))
           (displayln to-play)
           |#
           (sleep .1)
           ;; occasionally wait for the clock to run out for testing the time-out feature of the server
           (when (< (random) 0.03) (sleep 2.5))
           ;; send the client's move to the server
           (displayln (random-move board-state) oprt) (flush-output oprt) ;send move to server
           (net-play-moves iprt oprt)]
          [else gstatus])))

;; net-play-game: string -> symbol
;; connect to a GomokuServer running on the given host and play a game
(define (net-play-game server-host)
  (let*-values ([(inprt oprt) (tcp-connect server-host GOMOKUPORT)]
                [(ignore1) (printf "net-play-game: connected to ~a~n" server-host)]
                [(result) (net-play-moves inprt oprt)])
    (close-output-port oprt)
    (close-input-port inprt)
    result))

(net-play-game "localhost")