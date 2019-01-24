#lang racket

(require "data.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")
;; NeSegs is one of:
;; - (cons Posn empty)
;; - (cons Posn NeSegs)

(define/ctc-helper ne-segs? (and/c snake-segs? cons?))

;; cut-tail : NeSegs -> Segs
;; Cut off the tail.
(define/contract (cut-tail segs)
  (configurable-ctc
   [max (->i ([segs ne-segs?])
             [result (segs)
                     (snake-segs=?/c (drop-right segs 1))])]
   [types (ne-segs? . -> . snake-segs?)])

  (let ([r (cdr segs)])
    (cond [(empty? r) empty]
          [else (cons (car segs) (cut-tail r))])))

(provide
 cut-tail)
