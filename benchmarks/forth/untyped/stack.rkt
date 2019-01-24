#lang racket/base

(require racket/contract
         racket/list
         "../../../ctcs/precision-config.rkt"
         (only-in racket/function curry)
         (only-in "../../../ctcs/common.rkt"
                  stack?
                  list-with-min-size/c
                  equal?/c))

;; TODO make stacks be objects ?

;; Forth stacks,
;;  data definition & operations

(provide
  ;; type Stack = List
  ;;
  ;; Notation:
  ;;  []    = empty stack
  ;;  x::xs = item 'x' on top of the stack 'xs'
  ;;
  ;; Operations will raise an exception with message "empty stack"
  ;;  if called on a stack with too few elements.

  stack-drop
  ;; (-> x::xs xs)
  ;; Drop the item on top of the stack

  stack-dup
  ;; (-> x::xs x::x::xs)
  ;; Duplicate the item on top of the stack

  stack-init
  ;; (-> Stack)
  ;; Initialize an empty stack

  stack-over
  ;; (-> x::y::xs x::y::x::xs)
  ;; Duplicate the item on top of the stack, but place the duplicate
  ;;  after the 2nd item on the stack.

  stack-pop
  ;; (-> x::xs (Values x xs))
  ;; Pop the top item from the stack, return both the item and the new stack.

  stack-push
  ;; (-> xs x x::xs)
  ;; Push an item on to the stack

  stack-swap
  ;; (-> x::y::xs y::x::xs)
  ;; Swap the positions of the first 2 items on the stack.
)

;; =============================================================================

(define/ctc-helper stackof listof)
(define/ctc-helper non-empty-stack? (and/c stack?
                                (not/c empty?)))
(define/ctc-helper stack-with-min-size/c list-with-min-size/c)

(define/contract (list->stack xs)
  (configurable-ctc
   [max (->i ([xs list?])
             [result (xs) (equal?/c xs)])]
   [types (list? . -> . stack?)])

  (for/fold ([S (stack-init)])
            ([x (in-list (reverse xs))])
    (stack-push S x)))

(define/contract (stack-drop S)
  (configurable-ctc
   [max (->i ([S non-empty-stack?])
             [result (S) (equal?/c (rest S))])]
   [types (stack? . -> . stack?)])

  (let-values ([(_v S+) (stack-pop S)])
    S+))

(define/contract (stack-dup S)
  (configurable-ctc
   [max (->i ([S non-empty-stack?])
             [result (S) (equal?/c (cons (first S) S))])]
   [types (stack? . -> . stack?)])

  (let-values ([(v S+) (stack-pop S)])
    (stack-push (stack-push S+ v) v)))

(define/contract (stack-init)
  (configurable-ctc
   [max (-> (and/c stack? empty?))]
   [types (-> stack?)])

  '())

(define/contract (stack-over S)
  (configurable-ctc
   [max (->i ([S (stack-with-min-size/c 2)])
             [result (S)
                     (equal?/c
                      (cons (first S)
                            (cons (second S)
                                  (cons (first S)
                                        (rest (rest S))))))])]
   [types (stack? . -> . stack?)])

  (let*-values ([(v1 S1) (stack-pop S)]
                [(v2 S2) (stack-pop S1)])
    (stack-push (stack-push (stack-push S2 v1) v2) v1)))

(define/contract (stack-pop S)
  (configurable-ctc
   [max (->i ([S stack?])
             (values
              [first-result (S) (equal?/c (first S))]
              [second-result (S) (equal?/c (rest S))]))]
   [types (stack? . -> . (values any/c stack?))])

  (if (null? S)
      (raise-user-error "empty stack")
      (values (car S) (cdr S))))

(define/contract (stack-push S v)
  (configurable-ctc
   [max (->i ([S stack?]
              [v any/c])
             [result (S v)
                     (equal?/c (cons v S))])]
   [types (stack? any/c . -> . stack?)])

  (cons v S))

(define/contract (stack-swap S)
  (configurable-ctc
   [max (->i ([S (stack-with-min-size/c 2)])
             [result (S)
                     (equal?/c
                      (cons (second S)
                            (cons (first S)
                                  (rest (rest S)))))])]
   [types (stack? . -> . stack?)])

  (let*-values ([(v1 S1) (stack-pop S)]
                [(v2 S2) (stack-pop S1)])
    (stack-push (stack-push S2 v1) v2)))

