#lang racket

;(require (submod "state-machine.rkt" testing) rackunit)


(define (is-non-zero-digit? symbol)
  (member symbol '(1 2 3 4 5 6 7 8 9)))

(define-syntax :NOT-0:
  #'(? is-non-zero-digit?))


(define result
  (match 0
    [:NOT-0: 'wrong]
    [0 'right]))
