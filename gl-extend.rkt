#lang racket

;; gl-extend.rkt
;; a few macros to ease the use of GL imperatives

(provide with-gl-matrix)

;; ============================================================
(require sgl)

(define-syntax with-gl-matrix
  (syntax-rules ()
    [(_ f ...) (begin
                 (gl-push-matrix)
                 f ...
                 (gl-pop-matrix))]))
