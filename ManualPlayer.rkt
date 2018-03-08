#lang racket


;;---------------------------------------
;;  GOMOKU CLIENT
;;---------------------------------------

(define GOMOKUPORT 17033)

;;-------------------------------------------------------------------------------------------
;; protocol: See GomokuServer.rkt for protocol
;;-------------------------------------------------------------------------------------------


;; read-board: input-port -> (listof string)
;; read a SQUARE-board from the port, packaging it as list of rows (top to bottom) where each row is a string
(define (read-board iprt)
  ;(read-bytes 1)
  (let ([first-row (read-line iprt)])
    (printf "~s~%" first-row)
    (cons first-row
          (for/list ([r (sub1 (string-length first-row))]) ;; assume board is square
            ;(read-bytes 1)
            (let ([a-line (read-line iprt)])
              (printf "~s~%" a-line)
              a-line)))))

;; net-play-moves: input-port output-port -> symbol
;; plays a networked GOMOKU game using the given socket ports to read game-states and write move-pairs
;; finally returns one of 'win, 'lose, or 'draw
(define (net-play-moves iprt oprt)
  (let* (;read game-status/game-state/to-play-player triple
         [gstatus (string->symbol (read-line iprt))]
         [ignore1 (printf "Game status: ~s~%" gstatus)]
         [board-state (read-board iprt)]
         [last-move (with-input-from-string (read-line iprt)
                      (lambda () (cons (read) (read))))]
         [to-play (read-line iprt)]
         [ignore2 (printf "Your color: ~s~%" to-play)])
    ;; repeat until game over
    (printf "Last move was: ~a~%" last-move)
    (cond [(symbol=? gstatus 'CONTINUING)
           ;; display the game-state information
           #|
           (displayln gstatus)
           (for ([row board-state]) (displayln row))
           (displayln to-play)
           |#
           (sleep .1)
           ;; occasionally wait for the clock to run out for testing the time-out feature of the server
           ;(when (< (random) 0.01) (sleep 2.5))
           ;; prompt for move
           (printf "~%Your move: ")
           ;; send the client's move to the server
           (fprintf oprt "~a ~a~%" (read) (read)) (flush-output oprt) ;send move to server
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