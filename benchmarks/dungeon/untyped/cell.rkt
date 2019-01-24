#lang racket/base
;; TODO use open?

(provide
 empty-cell%
 void-cell%
 door%
 vertical-door%
 horizontal-door%
 char->cell%
 wall%
 void-cell%
 cell%?
 cell%/c
 class-equal?
 cell%
 )

;; -----------------------------------------------------------------------------

(require
 racket/class
 "../base/un-types.rkt"
 racket/contract
 "../../../ctcs/precision-config.rkt"
 (only-in "../../../ctcs/common.rkt" class/c*)
 )
(require (only-in racket/function
                  curry))
(require (only-in "message-queue.rkt"
                  enqueue-message!
                  ))
(require (only-in racket/dict
                  dict-ref
                  dict-set!
                  dict-has-key?
                  ))

;; =============================================================================

(define-syntax-rule/ctc-helper (make-cell%/c-with self-id show-char
                                       free?/occupant-comparer)
  (class/c* (field/all [items list?]     ;; ll: never seems to
                       [occupant any/c]) ;; actually be used

            (all
             [open (->m void?)]
             [close (->m void?)]
             [free? (->m boolean?)])
            (inherit+super
             [show (->i ([self-id any/c])
                        [result (self-id) show-char])])
            (override
             [show (->m char?)])

            [free? (->i ([self-id any/c])
                        [result (self-id)
                                (curry free?/occupant-comparer
                                       (get-field occupant
                                                  self-id))])]))

(define/ctc-helper cell%/c (make-cell%/c-with self any/c (λ x #t)))
(define/ctc-helper cell%? (instanceof/c cell%/c))

(define/contract cell% ; some kind of obstacle by default
  (configurable-ctc
   [max (make-cell%/c-with self #\* equal?)]
   [types cell%/c])
  (class object%
    (inspect #f)
    (init-field [items    '()]
                [occupant #f]) ; player, monster, etc.
    (define/public (free?)
      #f)
    (define/public (show)
      #\*) ; for debugging
    (define/public (open)
      (enqueue-message! "Can't open that."))
    (define/public (close)
      (enqueue-message! "Can't close that."))
    (super-new)))

;; maps printed representations to cell classes
;; for map parsing
(define/contract chars->cell%s
  (configurable-ctc
   [max (hash/c char? cell%/c)]
   [types hash?])
  (make-hash))

;; Workaround for bug(?) in class comparison:
;; (subclass? cell% (dict-ref chars->cell%s #\*)) => #f
(define/ctc-helper (class-equal? a% b%)
  (and (subclass? a% b%)
       (subclass? b% a%)))

(define/contract (register-cell-type! c% char)
  (configurable-ctc
   [max (->i ([c% cell%/c]
              [char char?])
             [result void?]
             #:post (c% char) (class-equal? (dict-ref chars->cell%s char (λ x (void)))
                                            c%))]
   [types (cell%/c char? . -> . void?)])

  (dict-set! chars->cell%s char c%))


(define/contract (char->cell% char)
  (configurable-ctc
   [max (->i ([char (and/c char? (curry dict-has-key? chars->cell%s))])
             [result (char)
                     (and/c cell%/c
                            (curry class-equal? (dict-ref chars->cell%s char)))])]
   [types (char? . -> . cell%?)])

  (dict-ref chars->cell%s char))

(register-cell-type! cell% #\*)

(define/contract empty-cell%
  (configurable-ctc
   [max (make-cell%/c-with self
                           (or/c #\space
                                 (send (get-field occupant self)
                                       show))
                           (not/c equal?))]
   [types cell%/c])
  (class cell%
    (inspect #f)
    (inherit-field occupant)
    (define/override (free?)
      (not occupant))
    (define/override (show)
      (if occupant
          (send (or occupant (raise-user-error 'show)) show)
          #\space))
    (super-new)))
(register-cell-type! empty-cell% #\space)

(define/contract void-cell%
  (configurable-ctc
   [max (make-cell%/c-with self #\. equal?)]
   [types cell%/c])
  (class cell%
    (inspect #f)
    (define/override (show) #\.) ; for testing only
    (super-new)))
(register-cell-type! void-cell% #\.)

(define/contract wall%
  (configurable-ctc
   [max (make-cell%/c-with self #\X equal?)]
   [types cell%/c])
  (class cell%
    (inspect #f)
    (define/override (show) #\X) ; for testing only
    (super-new)))
(register-cell-type! wall% #\X)

(define/contract double-bar? boolean? #t)
(define-syntax-rule (define-wall name single-bar double-bar)
  (begin (define/contract name
           (configurable-ctc
            [max (make-cell%/c-with self
                                    (if double-bar? double-bar single-bar)
                                    equal?)]
            [types cell%/c])
           (class wall%
             (inspect #f)
             (define/override (show) (if double-bar? double-bar single-bar))
             (super-new)))
         ;; parse either kind
         (register-cell-type! name single-bar)
         (register-cell-type! name double-bar)
         (provide name)))
(define-wall pillar%           #\+     #\#)
(define-wall vertical-wall%    #\u2502 #\u2551)
(define-wall horizontal-wall%  #\u2500 #\u2550)
(define-wall four-corner-wall% #\u253c #\u256c)
(define-wall north-east-wall%  #\u2510 #\u2557)
(define-wall north-west-wall%  #\u250c #\u2554)
(define-wall south-east-wall%  #\u2518 #\u255d)
(define-wall south-west-wall%  #\u2514 #\u255a)
(define-wall north-tee-wall%   #\u252c #\u2566)
(define-wall south-tee-wall%   #\u2534 #\u2569)
(define-wall east-tee-wall%    #\u2524 #\u2563)
(define-wall west-tee-wall%    #\u251c #\u2560)

(define/contract door%
  (configurable-ctc
   [max (make-cell%/c-with self #\* (not/c equal?))]
   [types cell%/c])
  (class cell%
    (inspect #f)
    ;(init-field [open? #f])
    (inherit-field occupant)
    (define/override (free?)
      (and #;open? (not occupant)))
    (define/override (open)
      (if #t ;open?
          (enqueue-message! "The door is already open.")
          (void) #;(set! open? #t)))
    (define/override (close)
      (if #t ;open?
          (void) #;(set! open? #f)
          (enqueue-message! "The door is already closed.")))
    (super-new)))

(define/contract vertical-door%
  (configurable-ctc
   [max (make-cell%/c-with self
                           (or/c #\_
                                 (send (get-field occupant self)
                                       show))
                           (not/c equal?))]
   [types cell%/c])
  (class door%
    (inspect #f)
    (inherit-field #;open? occupant)
    (define/override (show)
      (if #t ;open?
          (if occupant (send (or occupant (raise-user-error 'vdoor)) show) #\_)
          #\|))
    (super-new)))
(register-cell-type! vertical-door% #\|)

(define/contract other-vertical-door%
  (configurable-ctc
   [max (make-cell%/c-with self
                           (or/c #\_
                                 (send (get-field occupant self)
                                       show))
                           (not/c equal?))]
   [types cell%/c])
  (class vertical-door%
    (inspect #f)
    (super-new #;[open? #t])))
(register-cell-type! other-vertical-door% #\_)

(define/contract horizontal-door%
  (configurable-ctc
   [max (make-cell%/c-with self
                           (or/c #\'
                                 (send (get-field occupant self)
                                       show))
                           (not/c equal?))]
   [types cell%/c])
  (class door%
    (inspect #f)
    (inherit-field #;open? occupant)
    (define/override (show)
      (if #t ;open?
          (if occupant (send (or occupant (raise-user-error 'hdoor)) show) #\')
          #\-))
    (super-new)))
(register-cell-type! horizontal-door% #\-)

(define/contract other-horizontal-door%
  (configurable-ctc
   [max (make-cell%/c-with self
                           (or/c #\'
                                 (send (get-field occupant self)
                                       show))
                           (not/c equal?))]
   [types cell%/c])

  (class horizontal-door%
    (inspect #f)
    (super-new #;[open? #t])))
(register-cell-type! other-horizontal-door% #\')

;; TODO chests, entry/exit
