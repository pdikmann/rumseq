#lang racket

;; midi ruckus
;; FIX
;; - tracks sometimes receive 'empty patterns' from board.
;;   reason: hold is enabled on-drag, which might trigger when cursor is already above different field 
;; TODO / wishlist
;; - bigger note editor (for easier note-edit)
;; - note velocity in note editor (via keypress (1,2,3)?)
;; - more notes in note editor (scrollable view?)
;; - more tracks (scrollable view?)
;; - syncable tracks (start/stop lock),
;; - chained tracks (groups? start/stop together OR play after one another)
;; - smaller board (current is more than big enough)
;; - saving patterns & tracks (inside images, e.g. png stego?) // serialize things


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
(make-panel-macro 1/2 ;(/ (window-width main-window) 2)  ; editor - right side
                  0
                  1/2 ;(/ (window-width main-window) 2)
                  1 ;(window-height main-window)
                  #:on-event editor-event
                  ;;#:on-char editor-char
                  #:on-paint draw-editor!)
(make-panel-macro 0                 ; board - top left
                  1/2               ;(/ (window-height main-window) 2)
                  1/2               ;(/ (window-width main-window) 2)
                  1/2               ;(/ (window-height main-window) 2)
                  #:on-event board-event
                  ;;#:on-char
                  #:on-paint draw-board!)
(make-panel-macro 0                     ; tracks - bottom left
                  40
                  1/2         ;(/ (window-width main-window) 2)
                  (- 1/2 40) ;(- (/ (window-height main-window) 2) 40)
                  #:on-event tracks-event
                  #:on-char tracks-char
                  #:on-paint draw-tracks!)
(make-panel-macro 0                     ; tempo - very bottom left
                  0
                  1/2                ;(/ (window-width main-window) 2)
                  40
                  #:on-event tempo-event
                  #:on-char tempo-char
                  #:on-paint draw-tempo!)
(define full-area (gl-area 0
                           0
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

;; TODO further abstract/generalize into e.g. gl-view (view that has gl-area, on-event and on-char hooks)
(define (route-event e)
  (route-to-panel 'on-event e))

;; TODO think about keyboard interface
(define (route-char e)
  (route-to-panel 'on-char e))

;; ============================================================ to go
(send canvas paint-with draw-views!)
(send canvas on-event-with route-event)
(send canvas on-char-with route-char)
(send timer start 16)
