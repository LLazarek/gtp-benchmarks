#lang racket/base

(provide
  article
  random-between
  d6
  d20
  random-from
  random
  reset!
  random-result-between/c
)

(require
  (only-in racket/list first permutations)
  (only-in racket/file file->value)
  racket/contract
  (only-in "../../../ctcs/common.rkt"
           memberof/c)
  "../../../ctcs/precision-config.rkt"
)

;; =============================================================================

(define/contract orig
  any/c
  '(2 10 24 3 0 2 10 45 2 2 2 2 49 3 1 5 1 0 0 2 1 0 2 1 0 0 2 2 5 0 0 0 3 0 1 2
      0 3 0 0 2 2 0 2 2 0 0 3 0 0 2 0 3 1 0 2 0 0 1 1 0 2 0 0 3 0 0 1 2 0 3 1 0
      2 0 0 0 1 3 1 1 0 1 2 0 3 2 0 1 2 0 1 1 0 2 2 0 1 1 0 2 2 0 0 0 2 1 0 0 0 
      0 3 4 0 0 2 1 0 2 1 0 3 1 0 1 0 0 1 0 0 1 2 0 1 0 0 2 2 0 2 2 0 3 1 0 1 0 
      0 1 1 0 2 1 0 3 2 0 3 0 0 2 2 0 0 0 3 4 2 0 3 0 0 3 1 0 0 3 0 4 0 0 2 0 0 
      2 2 0 2 1 0 0 0 3 6 1 0 3 0 0 0 2 1 3 0 0 3 1 0 1 1 0 2 0 0 3 2 0 2 1 0 1
      2 0 0 3 0 2 2 0 2 2 0 2 2 0 1 1 0 3 1 0 2 1 0 1 2 0 0 2 0 3 1 0 1 1 0 2 2 
      0 2 2 0 1 5 3 3 2 1))
(define/contract r* any/c (box orig))

(define/contract (reset!)
  (configurable-ctc
   [max (->* ()
             void?
             #:post (equal? r* orig))]
   [types (-> void?)])

  (set-box! r* orig))

;; Non-specific ctc because this random stuff is rigged to be deterministic
(define/contract (random n)
  (configurable-ctc
   [max (any/c . -> . exact-nonnegative-integer?)]
   [types (any/c . -> . exact-nonnegative-integer?)])

  (begin0 (car (unbox r*)) (set-box! r* (cdr (unbox r*)))))

(define/ctc-helper (list+titlecases . los)
  (append los
          (map string-titlecase los)))

(define/contract (article capitalize? specific?
                 #:an? [an? #f])
  (configurable-ctc
   [max (->* (boolean? boolean?)
             [#:an? boolean?]
             (apply or/c (list+titlecases "the" "an" "a")))]
   [types (->* (boolean? boolean?)
               [#:an? boolean?]
               string?)])

  (if specific?
      (if capitalize? "The" "the")
      (if an?
          (if capitalize? "An" "an")
          (if capitalize? "A"  "a"))))


(define/ctc-helper (random-result-between/c min max)
  (and/c exact-nonnegative-integer?
         (>=/c min)
         (<=/c max)))

(define/contract (random-between min max) ;; TODO replace with 6.4's `random`
  (configurable-ctc
   [max (->i ([min exact-nonnegative-integer?]
              [max (min) (and/c exact-nonnegative-integer?
                                (>/c min))])
             [result (min max) (random-result-between/c min max)])]
   [types (exact-nonnegative-integer? exact-nonnegative-integer?
                                      . -> . exact-nonnegative-integer?)])

  (+ min (random (- max min))))

(define/contract (d6)
  (configurable-ctc
   [max (-> (random-result-between/c 1 7))]
   [types (-> exact-nonnegative-integer?)])

  (random-between 1 7))

(define/contract (d20)
  (configurable-ctc
   [max (-> (random-result-between/c 1 21))]
   [types (-> exact-nonnegative-integer?)])

  (random-between 1 21))

(define/contract (random-from l)
  (configurable-ctc
   [max (->i ([l (listof any/c)])
             [result (l) (memberof/c l)])]
   [types ((listof any/c) . -> . any/c)])

  (first (shuffle l)))

(define/contract (shuffle l)
  (configurable-ctc
   [max (->i ([l (listof any/c)])
             [result (l) (memberof/c (permutations l))])]
   [types ((listof any/c) . -> . (listof any/c))])

  (reverse l))
