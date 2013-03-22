#lang racket

;; tracks.rkt
;; - there is 1 stepper that controls the tempo of all tracks
;; - each track has a midi channel and an on/off switch
;; - tracks turn ON at the next beat and OFF when finished


(provide add-pattern
         draw-tracks!)

(require (planet evhan/coremidi)
         "step-timer.rkt"
         "models.rkt"
         "gl-geometry.rkt"
         "gl-texture.rkt"
         sgl
         racket/runtime-path)

;; ================================================== Model / Data
(define midi-connection (midi-open))

(define stepper (new stepper%))

(define tracks (for/list ([i 6])
                 (new midi-track%
                      [connection midi-connection])))

;; ============================================================ Functions
(define (add-pattern trk pt)
  #f)

;; ============================================================ GL
(define beat (quad #:color '(1 0 0 1)))
(define checker (quad #:color '(.95 .94 .63 1)))
(define-runtime-path font-path "gfx/font.png")
(define letters (for*/list ([y 3]
                            [x 16])
                  (let* ([xs (* 1/16 x)]
                         [xe (+ xs 1/16)]
                         [ys (* 1/3 y)]
                         [ye (+ ys 1/3)])
                    (quad font-path xs xe ys ye))))

(define (draw-tracks! wndw)
  ;; restrict drawing to WNDW area
  (apply gl-viewport (gl-area->list wndw))
  (apply gl-scissor (gl-area->list wndw))
  (gl-clear-color .96 .95 .71 1)
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
  ;; stripes
  (gl-push-matrix)
  (gl-scale (gl-area-width wndw)
            (/ (gl-area-height wndw) 6)
            1)
  (for ([i 3])
    (checker)
    (gl-translate 0 2 0))
  (gl-pop-matrix)
  ;; visible beat
  (gl-push-matrix)
  (gl-scale 10 10 0)
  (when (send stepper visible-beat?)
    (beat))
  (gl-pop-matrix)
  )

;; ============================================================ to go
(send stepper run)

