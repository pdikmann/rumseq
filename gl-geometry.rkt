#lang racket

(require "gl-window.rkt"
         "gl-texture.rkt"
         sgl
         sgl/gl
         sgl/gl-vectors)

(provide make-textured-quad
         aligned-textured-quad
         aligned-solid-quad
         make-solid-quad
         make-slider
         quad)

;; ============================================================ base geometries

(define (gl-quad)
  (gl-begin 'quads)
  (gl-tex-coord 0 0)
  (gl-vertex -1 -1 0)
  (gl-tex-coord 1 0)
  (gl-vertex  1 -1 0)
  (gl-tex-coord 1 1)
  (gl-vertex  1  1 0)
  (gl-tex-coord 0 1)
  (gl-vertex -1  1 0)
  (gl-end))


(define (gl-positive-quad [x-start 0] [x-end 1] [y-start 0] [y-end 1])
  (gl-begin 'quads)
  (gl-tex-coord x-start y-start)
  (gl-vertex    0 0 0)
  (gl-tex-coord x-end y-start)
  (gl-vertex    1 0 0)
  (gl-tex-coord x-end y-end)
  (gl-vertex    1 1 0)
  (gl-tex-coord x-start y-end)
  (gl-vertex    0 1 0)
  (gl-end))

;; (define (gl-positive-quad)
;;   (gl-begin 'quads)
;;   (gl-tex-coord 0 0)
;;   (gl-vertex    0 0 0)
;;   (gl-tex-coord 1 0)
;;   (gl-vertex    1 0 0)
;;   (gl-tex-coord 1 1)
;;   (gl-vertex    1 1 0)
;;   (gl-tex-coord 0 1)
;;   (gl-vertex    0 1 0)
;;   (gl-end))

(define (gl-lower-right-quad)
  (gl-begin 'quads)
  (gl-tex-coord 0 0)
  (gl-vertex 0 0 0)
  (gl-tex-coord 1 0)
  (gl-vertex  1 0 0)
  (gl-tex-coord 1 1)
  (gl-vertex  1  -1 0)
  (gl-tex-coord 0 1)
  (gl-vertex 0  -1 0)
  (gl-end))

;; ============================================================ gl-list building

(define (compile-list fn)
  (send canvas with-gl-context
        (lambda () (let ([object (gl-gen-lists 1)])
                (gl-new-list object 'compile)
                (fn)
                (gl-end-list)
                object))))

(define (compile-textured-list fn [tex #f])
    (compile-list (lambda ()
                  (gl-enable 'texture-2d)
                  (when tex
                    (glBindTexture GL_TEXTURE_2D
                                   (gl-vector-ref tex 0)))
                  (fn)
                  (gl-disable 'texture-2d))))

;; ============================================================ users

(define (make-textured-quad [tex #f])
  (compile-textured-list gl-quad tex))

(define (aligned-textured-quad [tex #f])
  (compile-textured-list gl-lower-right-quad
                         tex))

(define (make-solid-quad)
  (compile-list gl-quad))

(define (aligned-solid-quad)
  (compile-list gl-lower-right-quad))

(define (make-slider)
  (compile-list gl-lower-right-quad))

(define (quad [tex #f]
              [xs 0] ; texture coordinates: x start, x end, y start, y end
              [xe 1]
              [ys 0]
              [ye 1]
              #:color [color #f])
  (let ([gl-list (if tex
                     (compile-textured-list (lambda ()
                                              (when color (apply gl-color color))
                                              (gl-positive-quad xs xe ys ye))
                                            (tex-from-file tex))
                     (compile-list (lambda ()
                                     (when color (apply gl-color color))
                                     (gl-positive-quad))))])
    (lambda () (gl-call-list gl-list))))
