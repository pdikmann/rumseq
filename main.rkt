#lang racket

;; midi ruckus
;; TODO
;; - use timer as a self-perpetuating instance that modulo's through a supplied list
;;   until it is issues a stop/pause (it should remember its current timestep)

(require (planet evhan/coremidi)
         ;; gl
         "gl-window.rkt"
         "gl-timer.rkt"
         ;; other
         "step-timer.rkt"
         "models.rkt"
         "note-editor.rkt"
         sgl)

;; ============================================================ Functions
(define note-editor-gl-area (gl-area 320 0 320 240))
(define (test-viewports)
  ;; clear all
  ;;(gl-scissor 0 0 640 480)
  ;;(gl-clear-color 1 1 1 1)
  ;;(gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (draw-note-editor! note-editor-gl-area))

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

(send canvas paint-with test-viewports)
(send timer start 100)
