#lang racket

(provide (struct-out event)
         (struct-out note-event)
         (struct-out gl-area)
         gl-area->list)

(struct gl-area (x y width height)
        #:transparent)

(define (gl-area->list a)
  (list (gl-area-x a)
        (gl-area-y a)
        (gl-area-width a)
        (gl-area-height a)))

(struct event (time)
        #:transparent)

(struct note-event event (on note velocity)
        #:transparent)
