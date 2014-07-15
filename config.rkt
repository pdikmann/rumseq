#lang racket/base

(provide the-midi-port)

;; ----------------------------------------
;; MIDI PORT
;; #f         - auto (use first available midi port)
;; any number - use the corresponding midi port
(define the-midi-port #f)
