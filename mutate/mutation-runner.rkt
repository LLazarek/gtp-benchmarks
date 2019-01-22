#lang racket

(provide run-all-mutants/with-modules
         run-all-mutants/of-module
         run-with-mutated-module
         report-progress
         make-mutated-module-runner
         make-precision-config-module
         print-mutation
         mutate-module
         [struct-out run-status])

(require custom-load
         syntax/modread
         (only-in syntax/modresolve [resolve-module-path module-path->path])
         syntax/parse
         syntax/strip-context
         "mutate.rkt"
         "../ctcs/current-precision-setting.rkt"
         "sandbox-runner.rkt")

(define (module-path-resolve mod-path [load? #f])
  ((current-module-name-resolver) mod-path #f #f load?))

(define (read-module path)
  (check-module-form
   (with-module-reading-parameterization
     (λ () (with-input-from-file path
             (λ () (port-count-lines! (current-input-port)) (read-syntax)))))
   'ignored path))


;; Produce the mutated syntax for the module at the given path
(define (mutate-module module-file-path mutation-index)
  (syntax-parse (read-module module-file-path)
    #:datum-literals [module #%module-begin]
    [(module name lang (#%module-begin body ...))
     (define program-stx #'{body ...})
     (match-define (mutated-program program-stx/mutated mutated-id)
       (mutate-program/with-id program-stx mutation-index))
     (values
      (strip-context
       #`(module name lang
           (#%module-begin
            #,@program-stx/mutated)))
      mutated-id)]))

(define (make-precision-config-module precision-config)
  #`(module current-precision-setting racket
      (#%module-begin
       (provide current-precision-config
                precision-configs)
       (define precision-configs '(none types max))
       (define current-precision-config '#,precision-config))))

(define (make-mutated-module-runner main-module
                                    module-to-mutate
                                    mutation-index
                                    ctc-precision-config)
  (define main-module/path (path->string (simplify-path main-module)))
  (define to-mutate/path (path->string (simplify-path module-to-mutate)))

  (define main-module/module-path `(file ,main-module/path))
  (define-values (main-module/containing-directory ___1 ___2)
    (split-path (module-path->path main-module/module-path)))

  (define to-mutate/module-path `(file ,to-mutate/path))
  (define to-mutate/file-path (module-path->path to-mutate/module-path))
  (define-values (to-mutate/containing-directory _1 _2)
    (split-path to-mutate/file-path))
  (define-values (mutant-module-stx mutated-id)
    (mutate-module to-mutate/file-path mutation-index))

  (define precision-config/module-path
    '(file "../ctcs/current-precision-setting.rkt"))
  (define precision-config/file-path
    (module-path->path precision-config/module-path))
  (define-values (precision-config-containing-directory __1 __2)
    (split-path precision-config/file-path))
  (define precision-config/stx
    (make-precision-config-module ctc-precision-config))

  ;; Make racket/contract come from the same namespace so that
  ;; we can inspect contract violations thrown inside eval
  (define ns (make-base-namespace))
  (namespace-attach-module (current-namespace)
                           'racket/contract
                           ns)

  (define (run)
    (parameterize ([current-load/use-compiled
                    ;; Prevent loading from bytecode to ensure mutant is loaded
                    (make-custom-load/use-compiled
                     #:blacklist (curryr member
                                         (list to-mutate/file-path
                                               precision-config/file-path)))]
                   [current-namespace ns])
      (eval '(require "mutate.rkt"))

      ;; load the precision config make it impersonate the original
      ;; one, so that loading the original module loads the above
      ;; constructed config instead
      (parameterize
          ([current-load-relative-directory
            precision-config-containing-directory]
           ;; Note that this needs to be resolved *before*
           ;; parameterizing current-directory otherwise relative module
           ;; paths get messed up.
           [current-module-declare-name
            (module-path-resolve precision-config/module-path)]
           ;; Ensure relative load paths work
           [current-directory precision-config-containing-directory])
        (eval precision-config/stx))

      ;; Load the mutated module and make it impersonate the original
      ;; one, so that loading the original module loads the mutant
      ;; instead
      (parameterize
          ([current-load-relative-directory to-mutate/containing-directory]
           ;; Note that this needs to be resolved *before*
           ;; parameterizing current-directory otherwise relative module
           ;; paths get messed up.
           [current-module-declare-name
            (module-path-resolve to-mutate/module-path)]
           ;; Ensure relative load paths work
           [current-directory to-mutate/containing-directory])
        (eval mutant-module-stx))

      ;; Ensure relative load paths work
      (parameterize
          ([current-load-relative-directory main-module/containing-directory]
           [current-directory to-mutate/containing-directory])
        ;; Eval the main module
        (eval `(require ,main-module/module-path)))))

  (values run mutated-id))

(struct run-status (outcome blamed
                            mutated-module mutated-id
                            ctc-precision index)
  #:transparent)
;; run-status -> bool
(define (index-exceeded? rs)
  (equal? (run-status-outcome rs) 'index-exceeded))


(define report-progress (make-parameter #f))

;; run-status -> void
(define (display-run-status rs)
  (printf "
Run: mutated ~a in module ~a
with precision ~a, index ~a
Status: ~a
Blamed: ~a

"
          (run-status-mutated-id rs)
          (run-status-mutated-module rs)
          (run-status-ctc-precision rs)
          (run-status-index rs)
          (run-status-outcome rs)
          (run-status-blamed rs)))



(define (run-with-mutated-module main-module
                                 module-to-mutate
                                 mutation-index
                                 ctc-precision-config
                                 #:suppress-output? [suppress-output? #t]
                                 #:timeout/s [timeout/s (* 3 60)]
                                 #:memory/gb [memory/gb 3])
  (define (make-status status-sym [blamed #f] [mutated-id #f])
    (run-status status-sym
                blamed
                module-to-mutate
                mutated-id
                ctc-precision-config
                mutation-index))
  (with-handlers ([mutation-index-exception?
                   (λ (e) (make-status 'index-exceeded))])
    (define-values (run mutated-id)
      (make-mutated-module-runner main-module
                                  module-to-mutate
                                  mutation-index
                                  ctc-precision-config))
    (define (make-status* status-sym [blamed #f])
      (make-status status-sym blamed mutated-id))
    (define run/handled
      (λ _
        (with-handlers
          ([exn:fail:contract:blame? (compose
                                      (curry make-status* 'blamed)
                                      (match-lambda [`(function ,id) id]
                                                    [`(definition ,id) id]
                                                    [other other])
                                      blame-positive
                                      exn:fail:contract:blame-object)]
           [exn:fail:out-of-memory? (curry make-status* 'oom)]
           [exn? (curry make-status* 'crashed)])
          (run)
          (make-status* 'completed))))
    (run-with-limits run/handled
                     #:timeout/s timeout/s
                     #:timeout-result (make-status* 'timeout)
                     #:memory/gb memory/gb
                     #:oom-result (make-status* 'oom)
                     #:suppress-output? suppress-output?)))

(define/contract (run-all-mutants/of-module main-module
                                            module-to-mutate
                                            #:suppress-output? suppress-output?
                                            #:timeout/s timeout/s
                                            #:memory/gb memory/gb
                                            #:make-result
                                            [make-result identity]
                                            #:start-index
                                            [start-index 0])
  ([string?
    string?
    #:suppress-output? boolean?
    #:timeout/s number?
    #:memory/gb number?]
   [#:make-result ((listof run-status?) . -> . any/c)
    #:start-index natural?]
   . ->* .
   (listof any/c))

  (let loop ([index-so-far start-index]
             [results-so-far empty])
    (define results/this-index
      (for/list ([ctc-precision precision-configs])
        (when (report-progress)
          (displayln "."))
        (run-with-mutated-module main-module
                                 module-to-mutate
                                 index-so-far
                                 ctc-precision
                                 #:suppress-output? suppress-output?
                                 #:timeout/s timeout/s
                                 #:memory/gb memory/gb)))
    (cond [(index-exceeded? (first results/this-index))
           (flatten (reverse results-so-far))]
          [else
           (when (report-progress)
             (displayln "-------------------------")
             (for-each display-run-status results/this-index)
             (displayln "-------------------------"))
           (loop (add1 index-so-far)
                 (cons (make-result results/this-index)
                       results-so-far))])))

(define/contract (run-all-mutants/with-modules main-module
                                               mutatable-modules
                                               #:suppress-output?
                                               [suppress-output? #t]
                                               #:timeout/s
                                               [timeout/s (* 3 60)]
                                               #:memory/gb
                                               [memory/gb 3]
                                               #:make-result
                                               [make-result identity])
  ([string? (listof string?)]
   [#:suppress-output? boolean?
    #:timeout/s number?
    #:memory/gb number?
    #:make-result ((listof run-status?) . -> . any/c)]
   . ->* .
   (listof any/c))

  (flatten
   (map (λ (x)
          (run-all-mutants/of-module main-module x
                                     #:suppress-output? suppress-output?
                                     #:timeout/s timeout/s
                                     #:memory/gb memory/gb
                                     #:make-result make-result))
        mutatable-modules)))


;; for debugging
(define (print-mutation module-to-mutate mutation-index)
  (define-values (mutated-program-stx mutated-id)
    (mutate-module module-to-mutate mutation-index))
  (printf "--------------------\nMutated: ~a\n\n"
          mutated-id)
  (pretty-print (syntax->datum mutated-program-stx)))

(module+ test
  (require rackunit)
  (check-equal?
   (with-output-to-string
     (λ _ (run-with-mutated-module "a.rkt"
                                   "a.rkt"
                                   0
                                   'none
                                   #:suppress-output? #f)))
   "B
(c 5)
(d 1)
(a 0)
(b 1)
4
")

(check-equal?
 (with-output-to-string
   (λ _ (run-with-mutated-module "a.rkt"
                                 "b.rkt"
                                 0
                                 'none
                                 #:suppress-output? #f)))
 "B
(c -1)
(d 1)
(a 1)
(b 1)
-2
")

(check-match
 (run-with-mutated-module "a.rkt" "a.rkt" 2 'none
                          #:timeout/s 100
                          #:memory/gb 1)
 (run-status 'oom _ _ _ _ _))

(parameterize ([report-progress #t])
(check-match
 (run-all-mutants/with-modules "a.rkt" '("a.rkt" "b.rkt"))
 (list
   (run-status 'completed #f "a.rkt" 'a 'none 0)
   (run-status 'completed #f "a.rkt" 'a 'types 0)
   (run-status 'blamed 'a "a.rkt" 'a 'max 0)
   (run-status 'completed #f "a.rkt" 'b 'none 1)
   (run-status 'blamed 'b "a.rkt" 'b 'types 1)
   (run-status 'blamed 'b "a.rkt" 'b 'max 1)
   (run-status 'oom #f "a.rkt" 'foo 'none 2)
   (run-status 'oom #f "a.rkt" 'foo 'types 2)
   (run-status 'oom #f "a.rkt" 'foo 'max 2)
   (run-status 'completed #f "a.rkt" 'foo 'none 3)
   (run-status 'completed #f "a.rkt" 'foo 'types 3)
   (run-status 'blamed 'foo "a.rkt" 'foo 'max 3)
   (run-status 'completed #f "a.rkt" 'foo 'none 4)
   (run-status 'completed #f "a.rkt" 'foo 'types 4)
   (run-status 'blamed 'foo "a.rkt" 'foo 'max 4)
   (run-status 'completed #f "a.rkt" 'foo
               (or 'none 'types 'max)
               (or 5 6 7 8 9 10 11 12))
   ...
   (run-status 'completed #f "b.rkt" 'c 'none 0)
   (run-status 'blamed 'c "b.rkt" 'c 'types 0)
   (run-status 'blamed 'c "b.rkt" 'c 'max 0)
   (run-status 'crashed (? exn:fail:contract?) "b.rkt" 'd 'none 1)
   (run-status 'blamed 'd "b.rkt" 'd 'types 1)
   (run-status 'blamed 'd "b.rkt" 'd 'max 1)
   (run-status 'completed #f "b.rkt" 'd 'none 2)
   (run-status 'completed #f "b.rkt" 'd 'types 2)
   (run-status 'blamed 'd "b.rkt" 'd 'max 2)))))

(module+ main
  (parameterize ([report-progress #t])
    (run-all-mutants/with-modules "../benchmarks/dungeon/untyped/main.rkt"
                                  '("../benchmarks/dungeon/untyped/cell.rkt"
                                    "../benchmarks/dungeon/untyped/grid.rkt"
                                    "../benchmarks/dungeon/untyped/utils.rkt")
                                  #:suppress-output? #t
                                  #:timeout/s (* 3 60)
                                  #:memory/gb 3)))
