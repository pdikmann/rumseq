#lang racket

;; timer

(provide seq-timer%
         stepper%
         midi-track%)

(require (planet evhan/coremidi)
         (only-in racket/gui/base
                  timer%)
         "pattern.rkt"
         "models.rkt")

(define midi-track%
  (class object%
    (super-new)
    (init connection)
    ;; ================================ model
    (define midi-channel 1)
    (define sources '())        ; references to the patterns, in order
    (define unified (new pattern%))     ; single unified pattern
    (define raw-index 0)
    (define playing? #f)
    (define stopping? #f)
    ;; ================================ private
    (define (mod-len x)
      (modulo x (max 1 (send unified get-length))))

    (define (index)
      (mod-len raw-index))

    (define (inc-index)
      (set! raw-index
            (mod-len (+ raw-index 1))))

    (define (step)
      (when playing? (inc-index))
      (when (and stopping?
                 (= raw-index 0))
        (set! playing? #f)
        (set! stopping? #f)))

    (define (munge-sources!)
      (send unified set-length (for/sum ([pt sources])
                                 (send pt get-length))))
    ;; ================================ public
    (define/public (add-pattern pt)
      (set! sources (reverse (cons pt (reverse sources))))
      (munge-sources!))
    (define/public (inc-channel)
      (set! midi-channel (+ 1 (modulo midi-channel 10))))
    (define/public (dec-channel)
      (set! midi-channel (+ 1 (modulo (+ 8 midi-channel) 10))))
    (define/public (start) (set! playing? #t))
    (define/public (stop) (set! stopping? #t))
    (define/public (toggle) (if playing?
                                (set! stopping? #t)
                                (set! playing? #t)))
    (define/public (get-hook) step)
    (define/public (get-step) raw-index)
    (define/public (get-channel) midi-channel)
    (define/public (get-patterns) sources) ; TODO remove, debug only
    ))

(define stepper%
  (class timer%
    (super-new)
    (inherit start)
    ;; ================================ Model
    (define bpm 120)
    (define hooks '())
    (define visible-beat #f)
    ;; ================================ Private
    (define/override (notify)
      (for ([h hooks])
        (h))
      (set! visible-beat #t)
      (start (floor (/ 15000 bpm)) #t))
    ;; ================================ public
    (define/public (add-hook h)
      (set! hooks (cons h hooks)))
    (define/public (run)
      (start (floor (/ 15000 bpm)) #t))
    (define/public (set-bpm i)
      (set! bpm i))
    (define/public (get-bpm)
      bpm)
    (define/public (visible-beat?)
      (if visible-beat
          (begin (set! visible-beat #f)
                 #t)
          #f))
    ))

(define seq-timer%
  (class timer%
    (super-new)
    (inherit start)
    ;; ================================ model
    (define midi-connection #f)
    (define sequence '())
    (define index 0)
    ;; ================================ private
    ;; helpers
    (define (mod-len x)
      (modulo x (length sequence)))

    (define (current-event)
      (list-ref sequence (mod-len index)))

    (define (next-event)
      (list-ref sequence (mod-len (+ index 1))))

    (define (inc-index!)
      (set! index (mod-len (+ index 1))))

    (define (stop-all)
      (for ([e (filter (lambda (e) (and (note-event? e)
                                   (note-event-on e)))
                       sequence)])
        (note-off midi-connection 1
                  (note-event-note e)
                  (note-event-velocity e))))

    (define (notes->events notes [step 500] [init '()])
      (match notes
        ;; return sorted
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
                            (event-time b)))))]
        ;; recurse tail
        [(cons head tail)
         (notes->events tail
                        step
                        (append (list (note-event (* step (note-step head)) ; time
                                                  #t ; on
                                                  (note-value head) ; note
                                                  (note-velocity head)) ; velo
                                      (note-event (+ (* step (note-step head))
                                                     (* step (note-length head))) ; time
                                                  #f ; off
                                                  (note-value head) ; note
                                                  (note-velocity head))) ; velo
                                init))]))

    ;; main workhorse
    (define/override (notify)
      (when (not (empty? sequence))
        ;; pick indexed event, run it
        (let ([e (current-event)]
              [next-e (next-event)])
          (when (note-event? e)
            ((if (note-event-on e) note-on note-off)
             midi-connection 1
             (note-event-note e)
             (note-event-velocity e)))
          (inc-index!)
          ;; check time until next event, schedule
          (let ([delta (- (event-time next-e)
                          (event-time e))])
            (if (<= delta 0)
                ;; now
                (notify) ;; (start 0 #t)
                ;; later
                (start (floor delta) #t))))))

    ;; ================================ public
    (define/public (open-midi)
      (set! midi-connection (midi-open)))

    (define/public (close-midi)
      (midi-close midi-connection)
      (set! midi-connection #f))

    (define/public (use-sequence lst)
      (set! sequence lst))

    (define/public (use-notes lst)
      (when midi-connection
        (stop-all))
      (set! sequence (notes->events lst)))

    (define/public (run)
      (when (not midi-connection)
        (open-midi))
      (start 0 #t))))
