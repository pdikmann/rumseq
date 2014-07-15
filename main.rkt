#lang racket

;; main.rkt
;; rumseq - the Ridiculously Underpowered Midi SEQuencer
;; it's like SEQ24 without the features and for Mac OS X only (because of evhan/coremidi)
;; 
;; usage:
;; 1) edit config.rkt to set midi-port (default is auto-detection)
;; 2) $ racket main.rkt
;;
;; FIX
;; - tracks sometimes receive 'empty patterns' from board.
;;   reason: hold is enabled on-drag, which might trigger when cursor is already above different field 
;; TODO / wishlist (prioritized)
;; - note velocity in note editor (via keypress (1,2,3)?)
;; - saving patterns & tracks (inside images, e.g. png stego?) // serialize things
;; - more notes in note editor (scrollable view?)
;; - more tracks (scrollable view?)
;; - syncable tracks (start/stop lock),
;; - chained tracks (groups? start/stop together OR play after one another)
;; DONE
;; - bigger note editor (for easier note-edit)
;; - smaller board (current is more than big enough)


(require (planet evhan/coremidi)
         ;; gl
         "gl-window.rkt"
         "gl-timer.rkt"
         ;; other
         "models.rkt"
         "panels.rkt"
         ;; functionality
         "editor.rkt"
         "board.rkt"
         "tracks.rkt"
         "tempo.rkt"
         sgl)

;; ============================================================ Model
;; view panes (sub-windows)
(make-panel-macro 1/2 0 1/2 1          ; editor - right side
                  #:on-event editor-event
                  #:on-paint draw-editor!)
(make-panel-macro 0 1/2 1/2 1/2        ; board - top left              
                  #:on-event board-event
                  #:on-paint draw-board!)
(make-panel-macro 0 40 1/2 (- 1/2 40)  ; tracks - bottom left
                  #:on-event tracks-event
                  #:on-char tracks-char
                  #:on-paint draw-tracks!)
(make-panel-macro 0 0 1/2 40           ; tempo - very bottom left
                  #:on-event tempo-event
                  #:on-char tempo-char
                  #:on-paint draw-tempo!)
(define full-area (gl-area 0 0
                           (window-width main-window)
                           (window-height main-window)))

;; ============================================================ Function
(define (draw-views!)
  ;; clear all
  (apply gl-viewport (gl-area->list full-area))
  (apply gl-scissor (gl-area->list full-area))
  (gl-clear-color .96 .95 .71 1)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  ;;
  (draw-panels!))

;; ============================================================ to go
(send canvas paint-with draw-views!)
(send canvas on-event-with (curry route-to-panel 'on-event))
(send canvas on-char-with (curry route-to-panel 'on-char))
(send timer start 16)
