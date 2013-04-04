#lang racket

;; tracks.rkt
;; - there is 1 stepper that controls the tempo of all tracks
;; - each track has a midi channel and an on/off switch
;; - tracks turn ON at the next beat and OFF when finished

;; TODO
;; - more elegant inter-track drag'n'drop (pick single patterns instead of unified from midi-track%)

(provide add-pattern
         draw-tracks!
         tracks-event
         tracks-char
         stepper)

(require (planet evhan/coremidi)
         ;;"step-timer.rkt"
         "models.rkt"
         "stepper.rkt"
         "midi-track.rkt"
         "drag-holder.rkt"
         "gl-geometry.rkt"
         "gl-texture.rkt"
         "gl-letters.rkt"
         "gl-extend.rkt"
         sgl)

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
(define beat-indicator (quad #:color '(1 1 1 1)))
(define blackie (quad #:color '(0 0 0 1)))

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
  (gl-polygon-mode 'front-and-back 'fill)
  (with-gl-matrix (gl-scale (gl-area-width wndw)
                            (/ (gl-area-height wndw) 6)
                            1)
                  (for ([i 3])
                    (checker)
                    (gl-translate 0 2 0)))
  ;; visible beat
  ;; (with-gl-matrix (gl-scale 10 10 0)
  ;;                 (when (send stepper visible-beat?)
  ;;                   (beat)))
  ;; tracks
  (with-gl-matrix 
   (gl-scale (gl-area-width wndw)
             (/ (gl-area-height wndw) 6)
             1)
   (for ([tr tracks])
     (with-gl-matrix
      (gl-scale 1/68 1 1)
      ;; show track data (channel, playing)
      (with-gl-matrix
       (gl-scale 4 1 1)
       (gl-polygon-mode 'front-and-back 'fill)
       (gl-scale 1 0.5 1)
       (gl-translate 0 0.5 0)
       ;;(gl-color 1 1 1 1)
       ;;(gl-color .8 .9 1 1)
       (if (send tr get-playing?)
           (gl-color 1 0 0 1)
           (gl-color 0 0 0 1))
       (gl-font (number->string (send tr get-channel) 16)))
      ;; a bit to the right for the rest of the data ...
      (gl-translate 4 0 0)
      ;; beat indicator
      (with-gl-matrix
       (gl-translate (send tr get-step) 0 0)
       (beat-indicator))
      ;; patterns
      (for ([pt (send tr get-patterns)])
        ;; border
        (with-gl-matrix
         (gl-scale (send pt get-length) 1 1)
         (gl-polygon-mode 'front-and-back 'line)
         (blackie)
         (gl-polygon-mode 'front-and-back 'fill))
        ;; notes
        (with-gl-matrix
         (gl-scale 1 1/36 1)
         (for ([nt (send pt get-notes)])
           (with-gl-matrix
            (gl-translate (note-start nt)
                          (- 71 (note-value nt)) 0)
            (gl-scale (- (note-stop nt)
                         (note-start nt)) 1 1)
            (blackie))))                          ; end notes
        (gl-translate (send pt get-length) 0 0))) ; end patterns
     (gl-translate 0 1 0))))

;; ============================================================ Events
(define (tracks-event e x y)
  (let* ([select (floor (* y 6))]
         [selected (list-ref tracks select)]
         [in-upper-half? (= 0 (modulo (floor (* y 12)) 2))]
         [in-data-column? (< x 4/68)]
         [L-down? (send e button-down? 'left)]
         [L-up? (send e button-up? 'left)]
         [R-down? (send e button-down? 'right)]
         ;;[R-up? (send e button-up? 'right)]
         [drag? (send e dragging?)])
    (cond
     ;; toggle playing
     [(and L-down?
           in-data-column?)
      (send selected toggle)]
     ;; drag
     [(and drag?
           (not in-data-column?))
      (when (not (holding?))
        (lift-pattern (send selected get-unified-pattern)))]
     ;; drop pattern
     [(and L-up?
           (not in-data-column?))
      (when (holding?)
        (send (list-ref tracks select)
              add-pattern
              (send (drop-pattern!)
                    copy)))]
     ;; clear track
     [(and R-down?
           (not in-data-column?))
      (send selected clear)])))

(define (tracks-char e x y)
  (let* ([select (floor (* y 6))]
         [selected (list-ref tracks select)]
         [in-data-column? (< x 4/68)]
         [key (send e get-key-code)]
         [wheel-up? (eq? key 'wheel-up)]
         [wheel-down? (eq? key 'wheel-down)])
    (cond [(and wheel-up?
                in-data-column?)
           (send selected inc-channel)]
          [(and wheel-down?
                in-data-column?)
           (send selected dec-channel)])))

;; ============================================================ to go
(for ([mt tracks])
  (send stepper add-hook (send mt get-hook)))

(send stepper run)

