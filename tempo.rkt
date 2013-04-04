#lang racket

;; tempo.rkt
;; push the tempo

(provide (rename-out [area-draw! draw-tempo!])
         tempo-event)

(require "models.rkt"
         "tracks.rkt"
         "gl-letters.rkt"
         sgl)

(define beats-per-minute 120)
(define last-click-time 0)
(define intervals '())

(define (tempo-event e x y)
  (let* ([L-down? (send e button-down? 'left)]
         [R-down? (send e button-down? 'right)])
    (cond
     [R-down?
      (set! intervals '())]
     [L-down?
      (let* ([this-click-time (current-inexact-milliseconds)]
             [this-interval (- this-click-time
                               last-click-time)])
        (cond
         [(>= this-interval 1000)
          (set! intervals '())
          ;;(displayln "clear interval")
          ]
         [else
          (set! intervals (cons this-interval
                                intervals))
          (set! beats-per-minute (/ 60000 (/ (foldl + 0 intervals)
                                             (length intervals))))
          (send stepper set-bpm beats-per-minute)])
        (set! last-click-time
              this-click-time))])))

(define (area-draw! wndw)
  ;; restrict drawing to WNDW area
  (apply gl-viewport (gl-area->list wndw))
  (apply gl-scissor (gl-area->list wndw))
  (if (send stepper visible-beat?)
      (gl-clear-color 1 1 1 1)
      (gl-clear-color .8 .9 1 1))
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  ;; projection
  (gl-matrix-mode 'projection)
  (gl-load-identity)
  (gl-ortho 0 (gl-area-width wndw)      ; x left, right
            (gl-area-height wndw) 0     ; y bottom, top
            0 10)                       ; z
  (gl-translate 0 0 -1)
  ;; model view
  (gl-matrix-mode 'modelview)
  (gl-load-identity)
  ;;
  (gl-scale (gl-area-height wndw)
            (gl-area-height wndw)
            1)
  (gl-color 0 0 0 1)
  (gl-font (real->decimal-string beats-per-minute 0)))

;; ============================================================ to go
(send stepper set-bpm beats-per-minute)
