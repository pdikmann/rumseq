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

    ;; ================================ public
    (define/public (add-note step
                             value
                             [velocity 127]
                             [length 1])
      (hash-set! notes
                 (cons step value)
                 (note step value velocity length)))

    (define/public (remove-note step value)
      (hash-remove! notes
                    (cons step value)))
    (define/public (get-notes)
      (hash-values notes))))
