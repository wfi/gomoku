# gomoku

## Synopsis

Provides a Gomoku server that allows two player controllers to play each other.
The server enforces a time-limit on moves (defaults to two seconds).
The tournament wrapper runs round-robin best of *n* matches between a given list of players.
Although the server and tournament wrapper is written in Racket,
player controllers can be written in any language communicating with the server over sockets.
The communication protocol is specified in GomokuServer.rkt.

## Motivation

I use this in my AI and Machine Learning classes as part of assignments
where students have to write Gomoku players
(that either search or learn evaluation functions, respectively)
and then I play them all against each other in an in-class tournament.

## Running

Currently, you can set up a single stand-alone server with (serve-a-game (get-a-listener)).
Then have two players connect and play a game.
Repeat as desired.

The tournament needs a bit more set-up.
Players must be created as in the example found in tournament.rkt.
In this case, players must run locally on the server machine.

## Protocol

Connection: simply establish socket connection to port 17033.

Game state:
When it is the player's turn,
the server will send the following newline-terminated data elements:
* Game status.
Status value is one of: CONTINUING, WIN, LOSE, DRAW, FORFEIT-MOVE, or FORFEIT-TIME.
The last two values reflect an illegal move or exceeding the 2 second limit, respectively.
* Current game board configuration.
(currently only supports square boards)
A series of newline-terminated strings having as many characters
(not counting the newline) as the board dimension (width and height).
Subsequent to the first row of length n, there will be n-1 lines to follow
representing the rest of the rows of the board.
* The previous move that was made.
Two numbers representing the 0-based row and column of the move just made by the other player.
If this player is asked to make the first move of the game (i.e., no previous move),
the row and column values will both be -1.
* Which color is to play.
Either "x" or "o" for black or white, respectively.

Player move:
The player sends its move as row and column (again, 0-based)
on a single newline-terminated string.

## To Do

* convert graphics from draw to image package
* add better error handling in tournament.rkt when a client player crashes
* re-capture the standard output from a killed player after forfeit-time
* add comprehensive recording of results rather than just displaying the
result between each pairing of two players
* ~~have graphics show the winning series of pieces in a row~~
* ~~might need to kill process when forfeit-time happens client player crashes~~
