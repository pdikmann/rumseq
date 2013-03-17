#lang racket

;; midi ruckus
;; TODO
;; - use timer as a self-perpetuating instance that modulo's through a supplied list
;;   until it is issues a stop/pause (it should remember its current timestep)

(require (planet evhan/coremidi)
         (only-in racket/gui/base
                  timer%))

;; ============================================================ Model
(struct event (time)
        #:transparent)
(struct note-event event (on note velocity)
        #:transparent)

;; ============================================================ Functions
;; turn a list of '(start-step note-value length-in-steps) into events
(define (notes->events notes [step 500] [init '()])
  (match notes
    ['() (sort init
               (lambda (a b)
                 (if (= (event-time a)
                        (event-time b))
                     ;; at same time, note-off takes precedence
                     (if (and (not (note-event-on a))
                              (note-event-on b))
                         #t
                         #f)
                     ;; else
                     (< (event-time a)
                        (event-time b))))
               ;; #:key event-time
               )]
    [(cons head tail)
     (notes->events tail
                    step
                    (append (list (note-event (* step (car head)) ; time
                                              #t                  ; on
                                              (cadr head) ; note
                                              127)        ; velo
                                  (note-event (+ (* step (car head))
                                                 (* step (caddr head))) ; time
                                              #f          ; off
                                              (cadr head) ; note
                                              127))       ; velo
                            init))]))

;; a closure to play one note, then invoke TICKER for the next
(define (play-snip midi events end)
  (match events
    ['() (end)]                         ; should never happen
    [(list single)
     ((if (note-event-on single) note-on note-off)
      midi 1
      (note-event-note single)
      (note-event-velocity single))
     (end)]
    [(cons this others)
     ;; send note on or off, immediately
     ((if (note-event-on this) note-on note-off)
      midi 1
      (note-event-note this)
      (note-event-velocity this))
     ;; calc time till next event,
     ;; either self call right away
     ;; or use TICKER w/ a lambda-wrap 
     (let ([delta (- (event-time (first others))
                     (event-time this))])
       (if (= delta 0)
           (play-snip midi others end)
           (send ticker
                 tick-with
                 (lambda () (play-snip midi others end))
                 delta)))]))

(define (play-notes notes)
  (let ([midi (midi-open)])
    (play-snip midi
               notes
               (lambda () (midi-close midi)))))

;; a stepping timer that runs itself one
(define step-timer% (class timer%
                      (super-new)
                      (inherit start)
                      ;; replacable notification fn
                      (define notify-fn (lambda () (void)))
                      ;; self-exciting replacer
                      (define/public (tick-with fn time)
                        (set! notify-fn fn)
                        (start (floor time) #t))
                      ;; make callback use custom fn
                      (define/override (notify)
                        (notify-fn))))

(define ticker (new step-timer%))

;; ============================================================ to go
;; test
(let* ([bpm 148]
       [notes '((0 60 1)     ; '(start-at-beat, note, length-in-beats)
                (1 101 2)
                (2 30 1))]
       [sequence (notes->events notes (/ 60000 bpm))])
  (play-notes sequence))
