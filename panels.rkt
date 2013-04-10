#lang racket

;; panels.rkt
;; create & command the different views (editor, board, tracks, bpm) of the program

(provide make-panel-macro
         draw-panels!
         route-to-panel)

;; ============================================================ Data
(require "models.rkt"
         "gl-window.rkt"
         (for-syntax syntax/parse))

(struct panel (area      ; a gl-area
               on-event  ; (lambda (e x y) ...)
               on-char   ; (lambda (e x y) ...)
               on-paint  ; (lambda (gl-area) ...)
               ))

(define panels '())

;; ============================================================ Helpers
(define (sort-keyword-pairs kws vs)
  (let ([pairs (map list kws vs)])
    (sort pairs
          keyword<?
          #:key car)))

;; ============================================================ Macros
(define (recur-mult lst v)
  ;;(displayln lst)
  (match lst
    [(cons hd tl) (cons (recur-mult hd v)
                        (recur-mult tl v))]
    [(and x
          (? number?)
          (? (curry >= 1))) (* x v)]
    [x x]))

(define-syntax (rel-gl-area stx)
  (syntax-case stx ()
    [(_ f ...)
     #'(apply gl-area
              (for/list ([token '(f ...)]
                         [size (in-cycle
                                (in-list
                                 (list (window-width main-window)
                                       (window-height main-window))))])
                (eval (recur-mult token size))))]))

;; ============================================================ Functions
(define-syntax (make-panel-macro stx)
  (syntax-parse stx
                [(_ x y w h (~seq key:keyword val:expr) ...)
                 #'(let* ([sorted (sort-keyword-pairs (list (quote key) ...)
                                                      (list val ...))]
                          [keys (map car sorted)]
                          [vals (map cadr sorted)])
                     (keyword-apply make-panel
                                    keys
                                    vals
                                    (list (rel-gl-area x y w h))))]))

(define (make-panel area
                    #:on-event [on-event (lambda (e x y) #t)]
                    #:on-char [on-char (lambda (e x y) #t)]
                    #:on-paint [on-paint (lambda () #t)])
  (set! panels (cons (panel area
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
