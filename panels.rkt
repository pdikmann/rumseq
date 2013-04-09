#lang racket

;; panels.rkt
;; create & command the different views (editor, board, tracks, bpm) of the program

(provide make-panel
         draw-panels!
         route-to-panel)

;; ============================================================
(require "models.rkt")

(struct panel (area      ; a gl-area
               on-event  ; (lambda (e x y) ...)
               on-char   ; (lambda (e x y) ...)
               on-paint  ; (lambda (gl-area) ...)
               ))

(define panels '())

;; ============================================================
(define (make-panel x y width height
                    #:on-event [on-event (lambda (e x y) #t)]
                    #:on-char [on-char (lambda (e x y) #t)]
                    #:on-paint [on-paint (lambda () #t)])
  (set! panels (cons (panel (gl-area x y width height)
                            on-event
                            on-char
                            on-paint)
                     panels)))

(define (draw-panels!)
  (for ([pnl panels])
    ((panel-on-paint pnl) (panel-area pnl))))

(define (route-to-panel type e)
  (let ([x (send e get-x)]
        [y (send e get-y)])
    (for ([pnl panels])
      (when (gl-area-hit? (panel-area pnl) x y)
        (let-values ([(x y) (gl-area-relative-event-position (panel-area pnl) e)])
          ((case type
             [(on-event) (panel-on-event pnl)]
             [(on-char) (panel-on-char pnl)]) e x y))))))
