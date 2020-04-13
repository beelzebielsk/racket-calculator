#lang racket

(require racket/gui "state-machine.rkt")

(define state-machine (new calculator%))

(define (char->symbol character)
  (string->symbol
    (list->string (list character))))
(define (char->number character)
  (string->number
    (list->string (list character))))
(define (char-in-string character str)
  (member character (string->list str)))
(define (char->calc-input character)
  (cond
    [(char-in-string character "0123456789")
     (char->number character)]
    [else (char->symbol character)]))
(define frame (new frame% [label "Calculator"]))
(define calculator-panel%
  (class panel%
    (define/override (on-subwindow-char receiver event) 
      (define key-pressed (send event get-key-code))
      (display (format "Key '~a' was pressed." key-pressed))
      (cond
        [(char-in-string key-pressed "0123456789.+-*/=")
         (send-symbol-to-calculator (char->calc-input key-pressed))]
        [(eq? key-pressed #\return)
         (send-symbol-to-calculator '=)])
      #true)
    (super-new)))

(define calculator
  (new calculator-panel%
       [parent frame]
       [vert-margin 20]
       [horiz-margin 20]
       [style '(border)]
       [border 20]))
(define vert
  (new vertical-pane% [parent calculator]))
(define expression-bar
  (let* ((panel (new panel% 
                     [parent vert] 
                     [style '(border)]
                     [alignment '(left center)]
                     [stretchable-width #true]
                     [stretchable-height #false]))
         (msg (new message% [parent panel] [label ""])))
    msg))

(define (change-expression new-expression)
  (send expression-bar set-label new-expression))
(define (send-symbol-to-calculator symbol)
  (change-expression 
    (send state-machine take-action-and-return-next-expression-string symbol)))

; spacer
(new pane% 
     [parent vert]
     [min-height 20]
     [stretchable-height #false])

(define spots-in-grid
  (let* ((horizontal-button-spacing 10)
         (vertical-button-spacing 10)
         (grid (new vertical-pane% 
                    [parent vert]
                    [spacing vertical-button-spacing]))
         (rows (for/list ([i (in-range 5)])
                 (new horizontal-pane% [parent grid] [spacing horizontal-button-spacing])))
         (spots (for/list ([row rows])
                  (for/list ([i (in-range 4)])
                    (new panel% 
                         [parent row]
                         [min-width 50]
                         [min-height 50]
                         ;[style '(border)]
                         ;[stretchable-width #false]
                         ;[stretchable-height #false]
                         )))))
    spots))

(define (make-calc-button parent symbol)
  (new button%
       [parent parent]
       [stretchable-width #true]
       [stretchable-height #true]
       [label (~a symbol)]
       [callback 
         (λ _ (send-symbol-to-calculator symbol))]))

(define button-placement
  '((empty empty empty C)
    (7 8 9 +)
    (4 5 6 -)
    (1 2 3 ÷)
    (|.| 0 = ×)))

(for ([grid-row spots-in-grid]
      [symbol-row button-placement])
  (for ([spot grid-row]
        [symbol symbol-row])
    (when (not (eq? symbol 'empty))
      (make-calc-button spot symbol))))

(send frame show #true)

;digraph for ×: *X (or X*)
;digraph for ÷: -: (or :-)
