#lang racket

;; ----------------------------
;; Mode detection (given)
;; ----------------------------
(define prompt?
  (let ([args (current-command-line-arguments)])
    (cond
      [(= (vector-length args) 0) #t]
      [(string=? (vector-ref args 0) "-b") #f]
      [(string=? (vector-ref args 0) "--batch") #f]
      [else #t])))

;; ----------------------------
;; Parsing helpers
;; ----------------------------
(define (skip-ws chars)
  (cond
    [(empty? chars) '()]
    [(char-whitespace? (first chars)) (skip-ws (rest chars))]
    [else chars]))

(define (digit? c)
  (and (char? c) (char-numeric? c)))

(define (take-while pred chars)
  (cond
    [(empty? chars) (values '() '())]
    [(pred (first chars))
     (define-values (taken rem) (take-while pred (rest chars)))
     (values (cons (first chars) taken) rem)]
    [else (values '() chars)]))

;; parse-number: DIGITS ('.' DIGITS)?
(define (parse-number chars)
  (define cs (skip-ws chars))
  (when (empty? cs)
    (error 'parse-number "Invalid Expression"))
  (unless (digit? (first cs))
    (error 'parse-number "Invalid Expression"))

  (define-values (int-chars rem1)
    (take-while digit? cs))
  (when (empty? int-chars)
    (error 'parse-number "Invalid Expression"))

  (cond
    [(and (not (empty? rem1)) (char=? (first rem1) #\.))
     (define rem-after-dot (rest rem1))
     (define-values (frac-chars rem2)
       (take-while digit? rem-after-dot))
     (when (empty? frac-chars)
       (error 'parse-number "Invalid Expression"))

     (define num-str
       (list->string (append int-chars (list #\.) frac-chars)))
     (define n (string->number num-str))
     (unless n (error 'parse-number "Invalid Expression"))
     (values n rem2)]
    [else
     (define num-str (list->string int-chars))
     (define n (string->number num-str))
     (unless n (error 'parse-number "Invalid Expression"))
     (values n rem1)]))

;; parse-expr: numbers + unary '-' negation
(define (parse-expr chars history)
  (define cs (skip-ws chars))
  (when (empty? cs)
    (error 'parse-expr "Invalid Expression"))
  (define c (first cs))
  (cond
    [(char=? c #\-)
     (define-values (v rem) (parse-expr (rest cs) history))
     (values (- v) rem)]
    [(digit? c)
     (parse-number cs)]
    [else
     (error 'parse-expr "Invalid Expression")]))

;; parse exactly one expression; error if extra non-ws remains
(define (eval-line line history)
  (define chars (string->list line))
  (define-values (v rem) (parse-expr chars history))
  (define rem2 (skip-ws rem))
  (if (empty? rem2)
      v
      (error 'eval-line "Invalid Expression")))

;; ----------------------------
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
;; ----------------------------
(define (repl history next-id)
  (when prompt?
    (display "> ")
    (flush-output))
  (define line (read-line))
  (cond
    [(eof-object? line) (void)]
    [(string=? line "quit") (void)]
    [else
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (print-error "Invalid Expression")
                        (repl history next-id))])
       (let ([v (eval-line line history)])
         (print-result next-id v)
         (repl (cons v history) (add1 next-id))))]))

(module+ main
  (repl '() 1))
