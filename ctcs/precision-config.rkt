#lang racket/base

(require (for-syntax syntax/parse
                     racket/list
                     racket/base
                     (only-in racket/function
                              curryr))
         racket/contract
         (only-in racket/function curry)
         (only-in racket/match match))

(provide configurable-ctc
         define/ctc-helper
         define-syntax-rule/ctc-helper
         define-syntax/ctc-helper)

(define-for-syntax precision-configs '(none types max))
;; Must be a member of ^; modify to change all configurable-ctc precision levels
(define-for-syntax current-precision-config 'max)

;; Usage:
;; (configurable-ctc [<unquoted-precision-config> contract?] ...)
;;
;; No need to specify the 'none case: any unspecified cases will
;; become any/c when that configuration is used.
;;
;; Example:
;; (define/contract (f x)
;;   (configurable-ctc [max (-> number? string?)]
;;                     [types procedure?]))
(define-syntax (configurable-ctc stx)
  (syntax-parse stx
    [(_ [level ctc] ...)
     (let* ([levels (syntax->datum #'(level ...))]
            [ctcs (flatten (syntax->list #'(ctc ...)))]
            [current-level-index (index-of levels
                                           current-precision-config)]
            [current-ctc-stx (if (number? current-level-index)
                                 (list-ref ctcs current-level-index)
                                 #'any/c)])
       (begin
         (unless (andmap (curryr member precision-configs) levels)
           (error 'configurable-ctc
                  "Unknown precision config provided at ~a" stx))
         (with-syntax ([ctc-for-current-level current-ctc-stx])
           (syntax/loc current-ctc-stx
             ctc-for-current-level))))]))



;; -------------------- taint wrappers --------------------
(define-syntax (c-> stx)
  (syntax-parse stx
    #:datum-literals (any values)
    [(_ (~or (~seq mandatory-kw:keyword mandatory-kw-dom)
              mandatory-dom:expr)
        ... rng)
     #'(-> (wrap/c mandatory-dom)
           ...
           (~@ mandatory-kw (wrap/c mandatory-kw-dom))
           ...
           (wrap/c rng))]))

(define-syntax (c->* stx)
  (syntax-parse stx
    #:datum-literals (any values)
    [(_ ((~or (~seq mandatory-kw:keyword mandatory-kw-dom)
              mandatory-dom:expr)
         ...)
        ((~or (~seq optional-kw:keyword optional-kw-dom)
              optional-dom:expr)
         ...)
        (~optional (~seq (~datum #:pre) pre-cond))
        rng
        (~optional (~seq (~datum #:post) post-cond)))
     #'(->* ((wrap/c mandatory-dom)
             ...
             (~@ mandatory-kw (wrap/c mandatory-kw-dom))
             ...)
            ((wrap/c optional-dom)
             ...
             (~@ optional-kw (wrap/c optional-kw-dom))
             ...)
            (~? (~@ #:pre pre-cond))
            (wrap/c rng)
            (~? (~@ #:post post-cond)))]))



(define-syntax (c->i stx)
  (define-syntax-class dependencies
    #:description "dependent-dependencies"
    (pattern (depend-id:id ...)
             #:with
             untainted-shadow-bindings
             #'([depend-id (tainted-value* depend-id)] ...)))
  (define-syntax-class dependent-arg
    #:description "dependent-argument"
    (pattern [param:id (~optional depends:dependencies) param-ctc:expr]
             #:with
             expansion
             #'[param (~? (depends.depend-id ...))
                      ;; Shadow depend bindings with untainted
                      ;; versions of the binding
                      (let (~? (~@ depends.untainted-shadow-bindings)
                               ())
                        (wrap/c param-ctc))]))
  (define-syntax-class single-dependent-rng
    #:description "single-dependent-range"
    (pattern [result:id (~optional depends:dependencies) result-ctc:expr]
             #:with
             expansion
             #'[result (~? (depends.depend-id ...))
                       ;; Same shadowing as `dependent-arg`
                       (let (~? (~@ depends.untainted-shadow-bindings)
                                ())
                         (wrap/c result-ctc))]))

  (syntax-parse stx
    #:datum-literals (any values)
    [(_ (arg:dependent-arg ...)
        (~optional (~seq (~datum #:pre)
                         (pre-arg:id ...)
                         pre-cond:expr))
        (~or single-rng:single-dependent-rng
             (values values-rng:single-dependent-rng ...))
        (~optional (~seq (~datum #:post)
                         (post-arg:id ...)
                         post-cond:expr)))
     #'(->i (arg.expansion ...)
            (~? (~@ #:pre (pre-arg ...) pre-cond))
            (~? single-rng.expansion)
            (~? (values values-rng.expansion ...))
            (~? (~@ #:post (post-arg ...) post-cond)))]))

;; lltodo: note that the above doesn't work when `values` range
;; contracts don't refer to arguments. (???)

(define-syntax (wrap/c stx)
  (syntax-parse stx
    #:datum-literals (any values)
    [(_ any) #'any]
    [(_ (values c ...)) #'(values (wrap/c c ...))]
    [(_ s) #'(wrap s)]))

;; If it's not a flat contract, then
;; applying higher order ctcs to a value will produce a contract-wrapped value.
;; We may not want to mess with these, because the only thing that checks actual
;; values is flat contracts.
;; Except these contracts do check if they're given a value of the right shape
;; immediately.
;; So we should replace higher order contracts with a function that
;; first checks if the given value is tainted, and if so pushes the
;; contract down into the taint wrapper, and if not is just the normal
;; contract.
;; Self-recognizing value contracts need to be made into functions
;; that check for equality after unwrapping the taint.
(define (wrap ctc)
  (match ctc
    ;; Self-recognizing value contracts
    [(? (or/c symbol?
              boolean?
              keyword?
              null?))
     (make-recognizer/c eq? ctc)]
    [(or (? (or/c string? bytes? char?))
         +nan.0 +nan.f)
     (make-recognizer/c equal? ctc)]
    [(? number?)
     (make-recognizer/c = ctc)]
    [(? regexp?)
     (make-recognizer/c regexp-match? ctc)]

    ;; Flat contracts
    [(? flat-contract?)
     (λ (x) (ctc (tainted-value* x)))]

    ;; Higher order contracts
    [else
     ;; push the higher order contract down into the taint wrapper
     ;; Since a higher order contract will return the contracted value
     (λ (x) (tainted-map ctc x))]))

(define ((make-recognizer/c recognizer v) x)
  (recognizer v (tainted-value* x)))


(struct tainted (value taints) #:transparent)
(define (tainted-map f x)
  (if (tainted? x)
      (struct-copy tainted x [value (f (tainted-value x))])
      (f x)))
(define (tainted-value* x)
  (if (tainted? x) (tainted-value x) x))

;; tainted-bind: (A -> Tainted(B)) Tainted(A) -> Tainted(B)
;; Note: doesn't do any kind of tain preservation or anything
(define (tainted-bind f x)
  (tainted-value* (tainted-map f x)))

(module+ test
  (require rackunit)
  (define x (tainted 2 '(a b c)))

  (define-syntax-rule (assert e)
    (unless e (error 'assert "~a failed" 'e)))

  (assert (not (integer? x)))
  (assert ((wrap integer?) x))
  (assert ((wrap/c integer?) x))
  (define/contract x2 (wrap/c integer?) x)

  (define/contract (foo1 x)
    (c-> integer? integer?)
    ;; simulate builtins that work on tainted values
    (tainted-map (curry + 2) x))
  (assert (foo1 x))

  (define/contract (foo2 x [y 2])
    (c->* (integer?) (integer?)
          integer?)
    ;; simulate builtins that work on tainted values
    (tainted-map (curry + y) x))
  (assert (foo2 x))

  (define/contract (foo3 x y)
    (c->i ([x integer?]
           [y integer?])
          [result integer?])
    ;; simulate builtins that work on tainted values
    (tainted-map (curry + y) x))
  (assert (foo3 x 2))

  (define/contract (foo4 x y)
    (c->i ([x integer?]
           [y (x) (and/c integer? (<=/c x))])
          [result (x y)
                  (curry equal? (+ x y))])
    ;; simulate builtins that work on tainted values
    (tainted-bind (λ (x-val)
                    (tainted-map (curry + x-val) y))
                  x))
  (assert (foo4 x x))

  ;; Check literal value ctcs
  (define/contract (foo5 x y)
    (c->i ([x integer?]
           [y (x) (and/c integer? (<=/c x))])
          [result (x y)
                  (+ x y)])
    ;; simulate builtins that work on tainted values
    (tainted-bind (λ (x-val)
                    (tainted-map (curry + x-val) y))
                  x))
  (assert (foo5 x x)))


(define-syntax-rule (define-ctc-helpers [ctc-helper-id expansion] ...)
  (begin
    (define-syntax ctc-helper-id (make-rename-transformer #'expansion))
    ...))

(define-ctc-helpers
  [define/ctc-helper define]
  [define-syntax-rule/ctc-helper define-syntax-rule]
  [define-syntax/ctc-helper define-syntax])

