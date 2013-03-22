#lang racket

;; midi-track.rkt
;; a track that you can push patterns in.
;; driven by an external sequencer, it burps midi signals on (step)

(provide midi-track%)

(require (planet evhan/coremidi)
         "pattern.rkt"
         "models.rkt")

(define midi-track%
  (class object%
    (super-new)
    (init connection)
    ;; ================================ model
    (define midi-connection connection)
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

    (define (note-off? nt)
      (= (alt-note-stop nt) raw-index))

    (define (note-on? nt)
      (= (alt-note-start nt) raw-index))

    (define (step)
      (when playing?
        (let ([offs (filter note-off? (send unified get-notes))]
              [ons (filter note-on? (send unified get-notes))])
          ;; turn off first
          (for ([nt offs])
            (note-off midi-connection
                      midi-channel
                      (alt-note-value nt)
                      (alt-note-velocity nt)))
          ;; turn ons second
          (for ([nt ons])
            (note-on midi-connection
                     midi-channel
                     (alt-note-value nt)
                     (alt-note-velocity nt))))
        (inc-index))
      (when (and stopping?
                 (= raw-index 0))
        (set! playing? #f)
        (set! stopping? #f)))

    (define (munge-sources!)
      ;; for each pattern:
      ;; take each note
      ;; add it to unified
      ;; ! omit if start is beyond pattern length
      ;; + modify start / stop by current pattern offset
      ;; + modulo stop if it is after unified length
      (send unified set-length (for/sum ([pt sources])
                                 (send pt get-length)))
      (for/list ([i (length sources)])
        (let* ([pt (list-ref sources i)]
               [offset (for/sum ([pt (take sources i)])
                         (send pt get-length))])
          (for ([nt (send pt get-notes)])
            (when (<= (alt-note-start nt)
                      (send pt get-length))
              (send unified add-note
                    (+ offset (alt-note-start nt))
                    (modulo (+ offset (alt-note-stop nt))
                            (send unified get-length))
                    (alt-note-value nt)
                    (alt-note-velocity nt)))))))

    ;; ================================ public
    (define/public (clear)
      (set! sources '())
      (set! unified (new pattern%)))
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
    (define/public (get-playing?) playing?)
    (define/public (get-patterns) sources)))
