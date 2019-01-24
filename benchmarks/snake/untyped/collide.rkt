#lang racket
(require "data.rkt"
         "const.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")


;; snake-wall-collide? : Snake -> Boolean
;; Is the snake colliding with any of the walls?
(define/contract (snake-wall-collide? snk)
  (configurable-ctc
   [max (->i ([snk snake-type?])
             [result (snk)
                     (match snk
                       [(snake _ (cons h _)) (head-collide? h)]
                       [_ #f])])]
   [types (snake-type? . -> . boolean?)])

  (head-collide? (car (snake-segs snk))))

;; head-collide? : Posn -> Boolean
(define/contract (head-collide? p)
  (configurable-ctc
   [max (->i ([p posn-type?])
             [result (p)
                     (not (and (< 0 (posn-x p) BOARD-WIDTH)
                               (< 0 (posn-y p) BOARD-HEIGHT)))])]
   [types (posn-type? . -> . boolean?)])

  (or (<= (posn-x p) 0)
      (>= (posn-x p) BOARD-WIDTH)
      (<= (posn-y p) 0)
      (>= (posn-y p) BOARD-HEIGHT)))

(define/ctc-helper (truthy->bool x) (if x #t #f))
(define/ctc-helper memf? (compose truthy->bool memf))

;; snake-self-collide? : Snake -> Boolean
(define/contract (snake-self-collide? snk)
  (configurable-ctc
   [max (->i ([snk snake-type?])
             [result (snk)
                     (match snk
                       [(snake _ (cons h t))
                        (memf? (posn=?/c h) t)]
                       [_ #f])])]
   [types (snake-type? . -> . boolean?)])

  (segs-self-collide? (car (snake-segs snk))
                      (cdr (snake-segs snk))))

;; segs-self-collide? : Posn Segs -> Boolean
(define/contract (segs-self-collide? h segs)
  (configurable-ctc
   [max (->i ([h posn-type?]
              [segs snake-segs?])
             [result (h segs)
                     (if (empty? segs)
                         #f
                         (memf? (posn=?/c h) segs))])]
   [types (posn-type? snake-segs? . -> . boolean?)])

  (cond [(empty? segs) #f]
        [else (or (posn=? (car segs) h)
                  (segs-self-collide? h (cdr segs)))]))
(provide
 snake-wall-collide?
 snake-self-collide?)
