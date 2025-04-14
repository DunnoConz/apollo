#lang racket/base

;; game-logic.rkt
;; Game logic for a simple game

;; Require our math utilities
(require "math-utils.rkt")

;; Define a player structure
(define (make-player name x y z health)
  (list name x y z health))

;; Access player properties
(define (player-name player) (list-ref player 0))
(define (player-x player) (list-ref player 1))
(define (player-y player) (list-ref player 2))
(define (player-z player) (list-ref player 3))
(define (player-health player) (list-ref player 4))

;; Calculate distance between two players
(define (player-distance player1 player2)
  (distance (player-x player1) (player-y player1) (player-z player1)
            (player-x player2) (player-y player2) (player-z player2)))

;; Damage a player based on distance
(define (damage-player player attacker damage max-distance)
  (let ((dist (player-distance player attacker))
        (current-health (player-health player)))
    (if (< dist max-distance)
        (let* ((damage-factor (clamp (/ (- max-distance dist) max-distance) 0 1))
              (actual-damage (* damage damage-factor)))
          (make-player (player-name player)
                     (player-x player)
                     (player-y player)
                     (player-z player)
                     (clamp (- current-health actual-damage) 0 100)))
        player))) 