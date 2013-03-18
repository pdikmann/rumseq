#lang racket

;; a timer used to refresh the gl canvas.
;; is intentionally coupled to windows.rkt for use of auth-refresh.

(provide timer)

;; ------------------------------------------------------------
;; implementation

(require "gl-window.rkt"
         racket/gui)

(define frame-timer%
  (class timer%
    (super-new)
    (define frames 0)
    (define notify-fn (lambda () (send canvas auth-refresh)))
    (define/override (notify)
      (notify-fn)
      (set! frames (+ 1 frames)))
    (define/public (get-frames) frames)
    ;; set new event callback
    (define/public (notify-with fn)
      (set! notify-fn fn))))

(define timer (new frame-timer%
                   [interval #f]))
