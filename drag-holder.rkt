#lang racket

;; drag-holder.rkt
;; helps board.rkt and tracks.rkt to communicate
;; about which patterns are to be inserted where

(provide lift-pattern
         drop-pattern!
         holding?)

;; ============================================================
(require "gl-window.rkt"
         (only-in racket/gui
                  cursor%))


;; ============================================================
(define hold #f)

(define (lift-pattern pt)
  (set! hold pt)
  (send canvas set-cursor (make-object cursor% 'hand)))

(define (drop-pattern!)
  (begin0
      hold
    (set! hold #f)
    (send canvas set-cursor #f)))

(define (holding?)
  (if hold
      #t
      #f))
