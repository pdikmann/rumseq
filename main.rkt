#lang racket

;; midi ruckus
;; TODO
;; - use timer as a self-perpetuating instance that modulo's through a supplied list
;;   until it is issues a stop/pause (it should remember its current timestep)

(require (planet evhan/coremidi)
         "step-timer.rkt"
         "models.rkt")

;; ============================================================ Functions
;; turn a list of '(start-step note-value length-in-steps) into events
(define (notes->events notes [step 500] [init '()])
  (match notes
    ['() (sort init
               (lambda (a b)
                 (if (= (event-time a)
                        (event-time b))
                     ;; at same time, note-off takes precedence
                     (if (and (not (note-event-on a))
                              (note-event-on b))
                         #t
                         #f)
                     ;; else
                     (< (event-time a)
                        (event-time b))))
               ;; #:key event-time
               )]
    [(cons head tail)
     (notes->events tail
                    step
                    (append (list (note-event (* step (car head)) ; time
                                              #t                  ; on
                                              (cadr head) ; note
                                              127)        ; velo
                                  (note-event (+ (* step (car head))
                                                 (* step (caddr head))) ; time
                                              #f          ; off
                                              (cadr head) ; note
                                              127))       ; velo
                            init))]))

;; ============================================================ to go
(define seq-tick (new seq-timer%))

(let* ([bpm 148]
       [notes '((0 60 1)     ; '(start-at-beat, note, length-in-beats)
                (1 60 1)
                (2 82 2)
                (3 42 2)
                (5 30 1)
                (6 30 1))]
       [sequence (notes->events notes (/ 60000 bpm))])
  ;;(play-notes sequence)
  (send seq-tick use-sequence sequence)
  (send seq-tick open-midi)
  (send seq-tick run))

;;(send seq-tick stop)
;;(send seq-tick close-midi)
