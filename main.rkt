#lang racket

;; midi ruckus
;; TODO
;; - interactive note editor
;; - pipe note editor to seq-ticker
;; ...
;; - pattern collection
;; - tracks

(require (planet evhan/coremidi)
         ;; gl
         "gl-window.rkt"
         "gl-timer.rkt"
         ;; other
         "step-timer.rkt"
         "models.rkt"
         "editor.rkt"
         sgl)

;; ============================================================ window areas
(define full-area (gl-area 0
                           0
                           (window-width main-window)
                           (window-height main-window)))
(define editor-area (gl-area (/ (window-width main-window) 2)
                             0
                             (/ (window-width main-window) 2)
                             (/ (window-height main-window) 2)))

(define (draw-views!)
  ;; clear all
  (apply gl-viewport (gl-area->list full-area))
  (apply gl-scissor (gl-area->list full-area))
  (gl-clear-color .96 .95 .71 1)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  
  (draw-editor! editor-area))

(define (route-event e)
  (let ([x (send e get-x)]
        [y (send e get-y)])
    (when (gl-area-hit? editor-area x y)
      (let-values ([(x y) (gl-area-relative-event-position editor-area e)])
        (editor-event e x y)))))

;; ============================================================ to go
(define seq-tick (new seq-timer%))

(define (test)
  (let* ([bpm 148]
         [notes '((0 60 1)   ; '(start-at-beat, note, length-in-beats)
                  (1 60 1)
                  (2 82 2)
                  (3 42 2)
                  (5 30 1)
                  (6 30 1))]
         [sequence (notes->events notes (/ 60000 bpm))])
    ;;(play-notes sequence)
    (send seq-tick use-sequence sequence)
    (send seq-tick open-midi)
    (send seq-tick run)))

;;(send seq-tick stop)
;;(send seq-tick close-midi)

(send canvas paint-with draw-views!)
(send canvas on-event-with route-event)
(send timer start 100)
