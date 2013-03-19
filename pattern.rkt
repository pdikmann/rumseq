#lang racket

(provide pattern%)

;; pattern
;; notes - hashmap

(require "models.rkt")

(define pattern%
  (class object%
    (super-new)
    (define notes (make-hash))

    (define/public (add-note step value [velocity 127] [length 1])
      (hash-set! notes
                 (cons step value)
                 (note step value velocity length)))

    (define/public (remove-note step value)
      (hash-remove! notes
                    (cons step value)))
    (define/public (get-notes)
      notes)))
