#lang racket

(provide (struct-out event)
         (struct-out note-event)
         ;;
         (struct-out note)
         ;;
         (struct-out gl-area)
         gl-area->list
         gl-area-hit?
         gl-area-relative-event-position)

(require "gl-window.rkt")

;; ============================================================ gl-area
(struct gl-area (x y width height)
        #:transparent)

(define (gl-area->list a)
  (list (gl-area-x a)
        (gl-area-y a)
        (gl-area-width a)
        (gl-area-height a)))

(define (gl-area-hit? a x y)
  (and (x . >= . (gl-area-x a))
       (x . < . (+ (gl-area-x a)
                   (gl-area-width a)))
       (y . >= . (- (- (window-height main-window)
                       (gl-area-y a))
                    (gl-area-height a)))
       (y . < . (- (window-height main-window)
                   (gl-area-y a)))))

(define (gl-area-relative-event-position a e)
  (let ([x (send e get-x)]
        [y (send e get-y)])
    (values (/ (- x (gl-area-x a))
               (gl-area-width a))
            (/ (- y (- (- (window-height main-window)
                          (gl-area-y a))
                       (gl-area-height a)))
               (gl-area-height a)))))

;; ============================================================ events
(struct event (time)
        #:transparent)

(struct note-event event (on note velocity)
        #:transparent)

(struct note (step value velocity length)
        #:mutable #:transparent)
