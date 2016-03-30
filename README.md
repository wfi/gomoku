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

## To Do

* add better error handling in tournament.rkt when a client player crashes
* add comprehensive recording of results rather than just displaying the
result between each pairing of two players
