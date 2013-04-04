#lang racket

;; gl-letters.rkt
;; basic lettering ability

(provide gl-font)

;; ============================================================
(require racket/runtime-path
         sgl
         "gl-geometry.rkt")

(define-runtime-path font-path "gfx/font.png")

(define letters (for*/list ([y 3]
                            [x 16])
                  (let* ([xs (* 1/16 x)]
                         [xe (+ xs 1/16)]
                         [ys (* 1/3 y)]
                         [ye (+ ys 1/3)])
                    (quad font-path xs xe ys ye))))

(define assocs
  (apply hash
         (flatten
          (map list
               (string->list "0123456789abcdefghijklmnopqrstuvwxyz.!?_/<>üä   ")
               letters))))

(define (gl-font str)
  (gl-push-matrix)
  (for ([letter (string->list str)])
    ((hash-ref assocs letter))
    (gl-translate 1 0 0))
  (gl-pop-matrix))
