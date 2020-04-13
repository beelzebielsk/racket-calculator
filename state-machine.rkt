#lang racket

(provide calculator%)
(module* testing #f
  (provide (all-defined-out)))

(define shift-register%
  (class object%
    (inspect #false)
    (super-new)
    (define value 0)
    (define/public (clear) (set! value 0))
    (define/public (get-value) value)
    (define/public (set-value new-value) (set! value new-value))
    (define/public (push digit) (set! value (+ (* value 10) digit)))))

(define (in-list value lst)
  (if (list? (member value lst))
    #true
    #false))
(define (is-digit? symbol)
  (in-list symbol '(0 1 2 3 4 5 6 7 8 9)))
(define (is-non-zero-digit? symbol)
  (in-list symbol '(1 2 3 4 5 6 7 8 9)))
(define (is-operator? symbol)
  (in-list symbol '(+ - × ÷)))

;(define-syntax :DIGIT:
  ;#'(0 1 2 3 4 5 6 7 8 9))
;(define-syntax :NOT-0:
  ;#'(1 2 3 4 5 6 7 8 9))
;(define-syntax :OPERATOR:
  ;#'(+ - × ÷))

; Q: Why do these work as macros with match, but the above commented
; macros did not work as datums for case?
; NOTE: These didn't work either. I am not sure what precisely what
; behavior was produced or why.
(define-syntax :DIGIT:
  #'(? is-digit?))
(define-syntax :NOT-0:
  #'(? is-non-zero-digit?))
(define-syntax :OPERATOR:
  #'(? is-operator?))
;(define-match-expander :DIGIT:
  ;(lambda (stx)
    ;#'(? is-digit?)))
;(define-match-expander :NOT-0:
  ;(lambda (stx)
    ;#'(? is-non-zero-digit?)))
;(define-match-expander :OPERATOR:
  ;(lambda (stx)
    ;#'(? is-operator?)))

(define-syntax-rule
  (transition pattern next-state action ...)
  [pattern action ... next-state])


(define calculator%
  (class object%
    (super-new)
    (define accumulator-register (new shift-register%))
    (define operand-register (new shift-register%))
    (define current-operator (void))
    (define state 'start/clear)
    (define (calculate-expression)
      (define operator
        (case current-operator
          [(+) +]
          [(-) -]
          [(×) *]
          [(÷) /]))
      (define result
        (operator (send accumulator-register get-value)
                  (send operand-register get-value)))
      (send accumulator-register set-value result))
    (define (clear-calculator)
      (send accumulator-register clear)
      (send operand-register clear)
      (set! current-operator (void))
      'start/clear)
    (define/public (take-action symbol)
      (define (no-op) state)
      (cond
        [(eq? symbol 'C) (clear-calculator)]
        [else
          (case state
            [(start/clear)
             (match symbol
               [(? is-non-zero-digit?) 
                (send accumulator-register push symbol)
                '1st-operand]
               [_ (no-op)])]
            [(1st-operand)
             (match symbol
               [(? is-digit?)
                 (send accumulator-register push symbol)
                 '1st-operand]
               [(? is-operator?)
                 (set! current-operator symbol)
                 'got-op]
               [_ (clear-calculator)])]
            [(got-op)
             (match symbol
               [(? is-non-zero-digit?)
                 (send operand-register push symbol)
                 '2nd-operand]
               [(? is-operator?)
                 (set! current-operator symbol)
                 'got-op]
               [_ (no-op)])]
            [(2nd-operand)
             (match symbol
               [(? is-digit?)
                 (send operand-register push symbol)
                 '2nd-operand]
               ['= 
                 (calculate-expression)
                'show-answer]
               [(? is-operator?)
                 (calculate-expression)
                 (set! current-operator symbol)
                 'got-op/accumulate]
               [_ (no-op)])]
            [(show-answer)
             (match symbol
               ['= 
                (calculate-expression)
                'show-answer]
               [(? is-operator?)
                 (set! current-operator symbol)
                 'got-op/accumulate]
               [_ (no-op)])]
            [(got-op/accumulate)
             (match symbol
               [(? is-non-zero-digit?)
                (send operand-register clear)
                (send operand-register push symbol)
                 'accumulate-operand]
               [(? is-operator?)
                 (set! current-operator symbol)
                 'got-op/accumulate]
               [_ (no-op)])]
            [(accumulate-operand)
             (match symbol
               [(? is-digit?)
                 (send operand-register push symbol)
                 'accumulate-operand]
               ['=
                 (calculate-expression)
                 'show-answer]
               [(? is-operator?)
                 (calculate-expression)
                 (set! current-operator symbol)
                 'got-op/accumulate]
               [_ (no-op)])])]))
    (define/public (get-expression-string)
      (case state
        [(start/clear) ""]
        [(1st-operand) (~a (send accumulator-register get-value))]
        [(got-op) 
         (~a #:separator " "
             (send accumulator-register get-value) current-operator)]
        [(2nd-operand) 
         (~a #:separator " "
             (send accumulator-register get-value) 
             current-operator
             (send operand-register get-value))]
        [(show-answer)
         (~a #:separator " "
             "=" (send accumulator-register get-value))]
        [(got-op/accumulate)
         (~a #:separator " "
             "=" (send accumulator-register get-value)
             current-operator)]
        [(accumulate-operand)
         (~a #:separator " "
           "=" (send accumulator-register get-value)
             current-operator (send operand-register get-value))]))
    (define/public (take-action-and-return-next-expression-string symbol)
      (set! state (take-action symbol))
      (get-expression-string))))

(module+ test
  (require rackunit)
  (define (generate-output-sequence symbol-sequence [c (void)])
    (define the-calculator
      (if (void? c) (new calculator%) c))
    (for/list ([symbol symbol-sequence])
              (send the-calculator take-action-and-return-next-expression-string symbol)))
  (define (test-each-case expected-results actual-results)
    (define (create-test-message case-no)
      (format "Case number ~a is incorrect" case-no))
    (for ([expected expected-results]
          [actual actual-results]
          [case-no (in-naturals)])
         (check-equal? expected actual (create-test-message case-no))))
  (test-case
    "predicates work"
    (check-true (is-digit? 0))
    (check-true (is-digit? 1))
    (check-false (is-non-zero-digit? 0))
    (check-true (is-non-zero-digit? 1)))
  (test-case
    "matches with predicates work"
    (define result
      (match 0
        [(? is-non-zero-digit?) 'wrong]
        [0 'right]))
    (check-equal? result 'right))
  (test-case
    "matches with macros work"
    (define result
      (match 0
        [:NOT-0: 'wrong]
        [0 'right]))
    (check-equal? result 'right))
  ;(test-case
    ;"transition macro"
    ;(define a-variable 0)
    ;(define (side-effect)
      ;(set! a-variable 1))
    ;(define result
      ;(match 0
        ;(transition (? is-non-zero-digit?) 'wrong (side-effect))
        ;[_ 'right]))
    ;(check-equal? result 'right)
    ;(check-equal? a-variable 1))
  (test-case
    "0s at start/clear are idempotent"
    (define symbol-sequence
      '(0 0 0))
    (define expected-output-sequence
      '( "" "" ""))
    (test-each-case expected-output-sequence
                    (generate-output-sequence symbol-sequence)))
  (test-case
    "final calculator behavior"
    (define symbol-sequence
      '(2 0 4 + 7 6 9 + 1 × 2 = C))
    (define expected-output-sequence
      '( "2"
         "20"
         "204"
         "204 +"
         "204 + 7"
         "204 + 76"
         "204 + 769"
         "= 973 +"
         "= 973 + 1"
         "= 974 ×"
         "= 974 × 2"
         "= 1948"
         ""))

    (test-each-case expected-output-sequence
                    (generate-output-sequence symbol-sequence))))
