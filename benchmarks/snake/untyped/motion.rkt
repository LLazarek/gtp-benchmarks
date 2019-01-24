#lang racket  
(require "data.rkt"
         "const.rkt"
         "motion-help.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(provide reset!)
(define/contract r pseudo-random-generator?
  (make-pseudo-random-generator))
(define/contract (reset!)
  (configurable-ctc
   [max (->* ()
             void?
             #:post
             (equal?
              (pseudo-random-generator->vector r)
              (let ([r* (make-pseudo-random-generator)])
                (parameterize ([current-pseudo-random-generator r*])
                  (random-seed 1324)
                  (pseudo-random-generator->vector r*)))))]
   [types (-> void?)])

  (parameterize ((current-pseudo-random-generator r))
    (random-seed 1324)))

;; world->world : World -> World
(define/contract (world->world w)
  (configurable-ctc
   [max (->i ([w world-type?])
             [result
              (w)
              (match w
                [(world (snake dir (cons h t)) food) #:when (posn=? h food)
                 (world/c (snake/c dir
                                   (compose (=/c (+ (length t) 2))
                                            length))
                          any/c)]
                [(world (snake dir segs) food)
                 (world/c (snake/c dir
                                   (compose (=/c (length segs))
                                            length))
                          (food=?/c food))])])]
   [types (world-type? . -> . world-type?)])

  (cond [(eating? w) (snake-eat w)]
        [else
         (world (snake-slither (world-snake w))
                (world-food w))]))
;; eating? : World -> Boolean
;; Is the snake eating the food in the world.
(define/contract (eating? w)
  (configurable-ctc
   [max (->i ([w world-type?])
             [result (w)
                     (match w
                       [(world (snake _ (cons p1 _)) p2) #:when (posn=? p1 p2)
                                               #t]
                       [_ #f])])]
   [types (world-type? . -> . boolean?)])

  (posn=? (world-food w)
          (car (snake-segs (world-snake w)))))
;; snake-change-direction : Snake Direction -> Snake
;; Change the direction of the snake.
(define/contract (snake-change-direction snk dir)
  (configurable-ctc
   [max (->i ([snk snake-type?]
              [dir snake-dir?])
             [result (snk dir)
                     (snake/c dir (snake-segs=?/c (snake-segs snk)))])]
   [types (snake-type? string? . -> . snake-type?)])

  (snake dir
         (snake-segs snk)))
;; world-change-dir : World Direction -> World
;; Change direction of the world.
(define/contract (world-change-dir w dir)
  (configurable-ctc
   [max (->i ([w world-type?]
              [dir snake-dir?])
             [result (w dir)
                     (match w
                       [(world (snake _ segs) food)
                        (world/c (snake/c dir (snake-segs=?/c segs))
                                 (food=?/c food))])])]
   [types (world-type? string? . -> . world-type?)])

  (world (snake-change-direction (world-snake w) dir)
         (world-food w)))


(define-syntax-rule/ctc-helper (save-random-excursion generator body ...)
  (let ([generator-state (pseudo-random-generator->vector generator)])
    (begin0
        (begin body ...)
      (vector->pseudo-random-generator! generator generator-state))))

;; snake-eat : World -> World
;; Eat the food and generate a new one.
(define/contract (snake-eat w)
  (configurable-ctc
   [max (->i ([w world-type?])
             [result (w)
                     (world/c (snake=?/c (snake-grow (world-snake w)))
                              (posn/c (integer-in 0 (sub1 BOARD-WIDTH))
                                      (integer-in 0 (sub1 BOARD-HEIGHT))))])]
   [types (world-type? . -> . world-type?)])

  (define i (add1 (random (sub1 BOARD-WIDTH) r)))
  (define j (add1 (random (sub1 BOARD-HEIGHT) r)))
  (world (snake-grow (world-snake w))
         (posn i j)
         
         #;(posn (- BOARD-WIDTH 1) (- BOARD-HEIGHT 1))))
(provide
 world-change-dir
 world->world)
