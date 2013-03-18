#lang racket

(require "gl-window.rkt"
         sgl/gl
         sgl/gl-vectors
         (only-in racket/draw
                  read-bitmap))

(provide tex-from-file)

;; lifted from collects/sgl/bitmap.rkt
(define (argb->rgba argb)
  (let* ((length (bytes-length argb))
         (rgba (make-gl-ubyte-vector length)))
    (let loop ((i 0))
      (when (< i length)
        (gl-vector-set! rgba (+ i 0) (bytes-ref argb (+ i 1)))
        (gl-vector-set! rgba (+ i 1) (bytes-ref argb (+ i 2)))
        (gl-vector-set! rgba (+ i 2) (bytes-ref argb (+ i 3)))
        (gl-vector-set! rgba (+ i 3) (bytes-ref argb (+ i 0)))
        (loop (+ i 4))))
    rgba))

(define (bitmap->argb bmp)
  (let* ((width (send bmp get-width))
         (height (send bmp get-height))
         (argb (make-bytes (* 4 width height) 255)))
    (send bmp get-argb-pixels 0 0 width height argb #f)
    argb))

(define (tex-from-file file) 
  (let* ([texnum (glGenTextures 1)]
         [bmp (read-bitmap (if (path? file) (path->string file) file))]
         [width (send bmp get-width)]
         [height (send bmp get-height)]
         [rgba (argb->rgba (bitmap->argb bmp))]
         )          
    (send canvas with-gl-context
          (lambda ()  (glBindTexture GL_TEXTURE_2D
                                (gl-vector-ref texnum 0))
             (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA width height 0
                           GL_RGBA GL_UNSIGNED_BYTE rgba)
             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST ;;NEAREST
                              )
             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)))
    texnum))
