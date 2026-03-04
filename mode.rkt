#lang racket

;; ----------------------------
;; Mode detection
;; ----------------------------
(define prompt?
  (let ([args (current-command-line-arguments)])
    (cond
      [(= (vector-length args) 0) #t]
      [(string=? (vector-ref args 0) "-b") #f]
      [(string=? (vector-ref args 0) "--batch") #f]
      [else #t])))
;;----------------------
;; Evaluating line
;;------------------------
(define (eval-line line history)
  (define chars (string->list line))
  (define-values (v rest) (parse-expr chars history))
  (define rest2 (skip-ws rest))
  (if (empty? rest2)
      v
      (error 'eval-line "Invalid Expression")))
;;---------------------------
;; Whitespace skipping
;;----------------------------
(define (skip-ws chars)
  (cond
    [(empty? chars) '()]
    [(char-whitespace? (first chars)) (skip-ws (rest chars))]
    [else chars]))
;; ----------------------------

;; ----------------------------
;; Character helpers
;; ----------------------------
(define (digit? c)
  (and (char? c) (char-numeric? c)))

(define (take-while pred chars)
  (cond
    [(empty? chars) (values '() '())]
    [(pred (first chars))
     (define-values (taken rest) (take-while pred (rest chars)))
     (values (cons (first chars) taken) rest)]
    [else (values '() chars)]))

;; ----------------------------
;; parse-number: reads a numeric literal from the front of chars
;; Grammar supported: DIGITS ('.' DIGITS)?
;; Returns (values number remaining-chars) or raises error
;; ----------------------------
(define (parse-number chars)
  (define cs (skip-ws chars))
  (when (empty? cs)
    (error 'parse-number "Invalid Expression"))
  (unless (digit? (first cs))
    (error 'parse-number "Invalid Expression"))

  ;; integer part
  (define-values (int-chars rest1)
    (take-while digit? cs))

  (when (empty? int-chars)
    (error 'parse-number "Invalid Expression"))

  ;; optional fractional part
  (cond
    [(and (not (empty? rest1)) (char=? (first rest1) #\.))
     (define rest-after-dot (rest rest1))
     (define-values (frac-chars rest2)
       (take-while digit? rest-after-dot))
     (when (empty? frac-chars)
       (error 'parse-number "Invalid Expression")) ; disallow "12."
     (define num-str
       (list->string (append int-chars (list #\.) frac-chars)))
     (values (string->number num-str) rest2)]
    [else
     (define num-str (list->string int-chars))
     (values (string->number num-str) rest1)]))

;; ----------------------------
;; parse-expr: parses ONE prefix expression from chars
;; Current version supports:
;;  - number literals
;;  - unary '-' negation
;; Returns (values value remaining-chars) or raises error
;; ----------------------------
(define (parse-expr chars history)
  (define cs (skip-ws chars))
  (when (empty? cs)
    (error 'parse-expr "Invalid Expression"))

  (define c (first cs))
  (cond
    ;; unary negation
    [(char=? c #\-)
     (define-values (v rest) (parse-expr (rest cs) history))
     (values (- v) rest)]

    ;; number
    [(digit? c)
     (parse-number cs)]

    [else
     (error 'parse-expr "Invalid Expression")]))
;; Output helpers
;; ----------------------------
(define (print-result id v)
  (display id)
  (display ": ")
  (display (real->double-flonum v))
  (newline))

(define (print-error msg)
  (display "Error: ")
  (display msg)
  (newline))

;; ----------------------------
;; Main loop
;; history: list of past results, newest-first
;; next-id: next id to assign (starts at 1)
;; ----------------------------
(define (repl history next-id)
  (when prompt?
    (display "> Plese enter your prefix expression (quit to close the program)")
    (flush-output))

  (define line (read-line))
  (cond
    [(eof-object? line) (void)]          ; end of input -> exit
    [(string=? line "quit") (void)]      ; exact "quit" -> exit
    [else
     (with-handlers ([exn:fail?
                      (λ (e)
                        (print-error "Invalid Expression")
                        (displayln (exn-message e))
                        (repl history next-id))])
       (define v (eval-line line history))   ; <- we’ll implement this
       (print-result next-id v)
       (repl (cons v history) (add1 next-id)))]))

;; ----------------------------


(module+ main
  (repl '() 1))
