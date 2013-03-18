#lang racket

;; main window and gl canvas with handy helper functions built-in

(provide canvas)

;; ------------------------------------------------------------
;; config
(struct window-config (width height label))
(define *config* (window-config 640
                                480
                                "midi"))

;; ------------------------------------------------------------
;; implementation
(require racket/gui
         sgl
         sgl/gl
         sgl/gl-vectors)

;; a window that kills racket on closing
(define killer-frame%
  (class frame%
    (super-new)
    (define/augment (on-close)
      (exit))))

;; the handy canvas
(define gl-canvas%
  (class canvas%
    (inherit with-gl-context
             swap-gl-buffers
             get-parent
             refresh)
    (super-new (style '(gl no-autoclear)))
    
    (define auth? #f)                   ; authorize refresh
    (define (auth-grant) (set! auth? #t))
    (define (auth-remove) (set! auth? #f))
    (define/public (auth-refresh)
      (auth-grant)
      (refresh))

    ;; camera helper math
    (define (glu-perspective fovy aspect znear zfar)
      (let ((f (/ 1 (tan (/ (* fovy (/ pi 180)) 2))))
            (g (- znear zfar)))
        (gl-mult-matrix
         (vector->gl-double-vector 
          (vector
           (/ f aspect) 0 0 0
           0 f 0 0
           0 0 (/ (+ znear zfar) g) -1
           0 0 (/ (* 2 znear zfar) g) 0)))))

    ;;initialization procedure (also occurs on resize)
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (gl-viewport 0 0 width height)
         ;; projection matrix
         (gl-matrix-mode 'projection)
         (gl-load-identity)
         ;; (glu-perspective 90
         ;;                  1 ;;(/ width height)
         ;;                  .1 10)
         ;; (gl-translate 0 0 -1)
         (gl-ortho 0 width
                   height 0
                   0 10)
         (gl-translate 0 0 -1)
         ;; model matrix
         (gl-matrix-mode 'modelview)
         (gl-load-identity)
         (gl-enable 'blend)
         (gl-blend-func 'src-alpha 'one-minus-src-alpha)
         ;;(gl-enable 'cull-face 'lighting 'texture-2d 'depth-test)
         (gl-enable 'scissor-test)
         )))

    ;;drawing function, replacable by new-draw-fn
    (define paint-fn
      (lambda ()
        (gl-clear-color 1 1 1 0.0)
        (gl-clear 'color-buffer-bit 'depth-buffer-bit)
        ;;
        (gl-matrix-mode 'modelview)
        (gl-load-identity)))

    (define on-event-fn (lambda (e) #t))
    (define on-char-fn (lambda (e) #t))

    ;; replace the draw-fn that is used in on-paint
    (define/public (paint-with fn) (set! paint-fn fn))

    ;; replace the fn called on any mouse event
    (define/public (on-event-with fn) (set! on-event-fn fn))

    ;; replace the fn called on any keyboard event
    (define/public (on-char-with fn) (set! on-char-fn fn))

    (define/override (on-paint)
      (when auth?
        (with-gl-context
         (lambda ()
           (paint-fn)
           (swap-gl-buffers)
           (gl-flush)))
        (auth-remove)))

    (define/override (on-event event) (on-event-fn event))
    (define/override (on-char event) (on-char-fn event))))

;; ------------------------------------------------------------
;; instantiation
(define canvas #f)
(define frame #f)

(define (window-init [cfg *config*])
  (when (not canvas)
    (set! frame
          (new killer-frame%
               [label (window-config-label cfg)]
               [width (window-config-width cfg)]
               [height (window-config-height cfg)]))
    (set! canvas (new gl-canvas%
                      [parent frame]
                      [min-width (window-config-width cfg)]
                      [min-height (window-config-height cfg)]))
    (send frame show #t)))

(window-init)
