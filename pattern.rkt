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
    (define/public (add-note start
                             value
                             [velocity 127])
      (let ([nt (alt-note start
                          (+ start 1)
                          value
                          velocity)])
        (hash-set! notes (cons start value) nt)
        nt))

    (define/public (sustain-note nt stop)
      (set-alt-note-stop! nt (max stop (+ 1 (alt-note-start nt)))))

    (define/public (remove-note step value)
      (hash-remove! notes (cons step value)))

    (define/public (set-notes hsh)
      (set! notes hsh))

    (define/public (set-length i)
      (set! length i))

    (define/public (copy)
      (let ([pt (new pattern%)])
        (send pt set-notes (hash-copy notes))
        (send pt set-length length)
        pt))

    (define/public (get-notes)
      (hash-values notes))

    (define/public (get-length)
      length)))
