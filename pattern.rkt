#lang racket

(provide pattern%)

;; pattern
;; notes - hashmap

(require "models.rkt")

(define pattern%
  (class object%
    (super-new)
    ;; ================================ model
    (define notes (make-hash))
    (define length 0)

    ;; ================================ public
    (define/public (add-note step
                             value
                             [velocity 127]
                             [length 1])
      (let ([nt (note step value velocity length)])
        (hash-set! notes (cons step value) nt)
        nt))

    (define/public (sustain-note nt step)
      (set-note-length! nt (+ (- step
                                 (note-step nt))
                              1)))

    (define/public (remove-note step value)
      (hash-remove! notes (cons step value)))

    (define/public (set-length i)
      (set! length i))

    (define/public (get-notes)
      (hash-values notes))

    (define/public (get-length)
      length)))
