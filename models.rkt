#lang racket

(provide (struct-out event)
         (struct-out note-event))

(struct event (time)
        #:transparent)
(struct note-event event (on note velocity)
        #:transparent)
