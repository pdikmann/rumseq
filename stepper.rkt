#lang racket

;; stepper.rkt
;; dutiful time stepper, for hooking midi-track%s into

(provide stepper%)

(require (only-in racket/gui/base
                  timer%))

(define stepper%
  (class timer%
    (super-new)
    (inherit start)
    ;; ================================ Model
    (define bpm 120)
    (define hooks '())
    (define visible-beat 0)
    ;; ================================ Private
    (define/override (notify)
      (for ([h hooks])
        (h))
      (set! visible-beat (modulo (+ 1 visible-beat) 4))
      (start (floor (/ 15000 bpm)) #t))
    ;; ================================ public
    (define/public (add-hook h)
      (set! hooks (cons h hooks)))
    (define/public (run)
      (start (floor (/ 15000 bpm)) #t))
    (define/public (set-bpm i)
      (set! bpm (inexact->exact (floor i))))
    (define/public (get-bpm)
      bpm)
    (define/public (visible-beat?)
      (= visible-beat 0)
      ;; (if visible-beat
      ;;     (begin (set! visible-beat #f)
      ;;            #t)
      ;;     #f)
      )))
