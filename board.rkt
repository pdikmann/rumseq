#lang racket

;; board.rkt holds all the patterns

(provide board-event
         draw-board!)

(require sgl
         "models.rkt"
         "drag-holder.rkt"
         "pattern.rkt"
         "editor.rkt"
         "gl-geometry.rkt")

;; ============================================================ Model / Data
(define patterns (for/list ([i 30]) (new pattern%)))

;; ============================================================ GL
;; geometry
(define checker (quad #:color '(.95 .94 .63 1)))
(define white (quad #:color '(1 1 1 1)))
(define gl-note (quad))

;; draw
(define (draw-board! wndw)
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
  ;; checkerboard, 10 by 3 fields
  (for* ([x (range 10)]
         [y (range 3)]
         #:when (even? (+ x y)))
    (gl-push-matrix)
    (gl-scale (/ (gl-area-width wndw) 10)
              (/ (gl-area-height wndw) 3)
              1)
    (gl-translate x y 0)
    (checker)
    (gl-pop-matrix))
  ;; patterns (including notes)
  (for ([pt patterns]
        [i (range (length patterns))]
        ;;#:when (not (empty? (send pt get-notes)))
        )
    (gl-push-matrix)
    ;; scale to slot size
    (gl-scale (/ (gl-area-width wndw) 10)
              (/ (gl-area-height wndw) 3)
              1)
    ;; translate to slot position
    (gl-translate (modulo i 10)
                  (floor (/ i 10))
                  0)
    ;; translate & scale to 90% (for border)
    (gl-translate 0.05 0.05 0)
    (gl-scale .9 .9 1)
    ;; scale to pattern length
    (gl-push-matrix)
    (gl-scale (/ (+ (send pt get-length) 1) 16) 1 1)
    ;; background
    (gl-polygon-mode 'front-and-back 'fill)
    (white)
    (gl-pop-matrix)
    ;; notes
    (gl-scale 1/16
              1/36
              1)
    (for ([nt (send pt get-notes)])
      (gl-push-matrix)
      (gl-translate (alt-note-start nt)
                    (- 71 (alt-note-value nt)) 0)
      (gl-scale (- (alt-note-stop nt)
                   (alt-note-start nt)) 1 1)
      ;; fill
      (gl-polygon-mode 'front-and-back 'fill)
      (gl-color 0 0 0 1)
      (gl-note)
      ;; outline
      ;; (gl-polygon-mode 'front-and-back 'line)
      ;; (gl-color 0 0 0 1)
      ;; (gl-note)
      (gl-pop-matrix))
    (gl-pop-matrix))
  )

;; ============================================================ Events
(define (board-event e x y)
  (let* ([select-x (floor (* x 10))]
         [select-y (floor (* y 3))]
         [L-down? (send e button-down? 'left)]
         [L-up? (send e button-up? 'left)]
         [R-down? (send e button-down? 'right)]
         ;;[R-up? (send e button-up? 'right)]
         [drag? (send e dragging?)]
         [select (+ (* select-y 10) select-x)]
         )
    (cond
     [L-down?
      (edit-pattern (list-ref patterns select))]
     [R-down?
      (set! patterns (append (take patterns select)
                             (list (new pattern%))
                             (drop patterns (+ select 1))))]
     [drag?
      (when (not (holding?))
        (lift-pattern (list-ref patterns select)))]
     [L-up?
      (when (holding?)
        (set! patterns (append (take patterns select)
                               (list (send (drop-pattern!) copy))
                               (drop patterns (+ select 1))))
        (edit-pattern (list-ref patterns select)))])))
