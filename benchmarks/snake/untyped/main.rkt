#lang racket

(require "data.rkt"
         "const.rkt"
         "handlers.rkt"
         "motion.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(define/contract (replay w0 hist)
  (configurable-ctc
   [max (world-type?
         (listof (or/c (list/c 'on-key string?)
                       (list/c 'on-tick)
                       (list/c 'stop-when)))
         . -> .
         void?)]
   [types (world-type? (listof (listof (or/c symbol? string?))) . -> . void?)])

  (reset!)
  (let loop ((w w0) (h hist))
    (if (empty? h)
        w
        (let ()
          (loop
           (match (car h)
             [`(on-key ,(? string? ke))
              (handle-key w ke)]
             [`(on-tick)
              (world->world w)]
             [`(stop-when)
              (game-over? w)
              w])
           (cdr h)))))
  (void))

(define/contract DATA
  any/c
  (with-input-from-file "../base/snake-hist.rktd" read))
(define/contract LOOPS
  (configurable-ctc
   [max (=/c 1)]
   [types natural?])
  1)

(define/contract (main hist)
  (configurable-ctc
   [max ((listof (or/c (list/c 'on-key string?)
                       (list/c 'on-tick)
                       (list/c 'stop-when)))
         . -> .
         void?)]
   [types ((listof (listof (or/c symbol? string?))) . -> . void?)])

  (define w0 (WORLD))
  (cond [(list? hist)
         (for ((_i (in-range LOOPS)))
           (replay w0 hist))]
        [else
         (error "bad input")]))

(time (main DATA))
