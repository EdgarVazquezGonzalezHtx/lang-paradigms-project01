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
     (define-values (taken rest) (take-while pred (rest chars)))
     (values (cons (first chars) taken) rest)]
    [else (values '() chars)]))

;; parse-number: DIGITS ('.' DIGITS)?
(define (parse-number chars)
  (define cs (skip-ws chars))
  (when (empty? cs)
    (error 'parse-number "Invalid Expression"))
  (unless (digit? (first cs))
    (error 'parse-number "Invalid Expression"))

  (define-values (int-chars rest1)
    (take-while digit? cs))
  (when (empty? int-chars)
    (error 'parse-number "Invalid Expression"))

  (cond
    [(and (not (empty? rest1)) (char=? (first rest1) #\.))
     (define rest-after-dot (rest rest1))
     (define-values (frac-chars rest2)
       (take-while digit? rest-after-dot))
     (when (empty? frac-chars)
       (error 'parse-number "Invalid Expression")) ; disallow "12."

     (define num-str
       (list->string (append int-chars (list #\.) frac-chars)))
     (define n (string->number num-str))
     (unless n (error 'parse-number "Invalid Expression"))
     (values n rest2)]
    [else
     (define num-str (list->string int-chars))
     (define n (string->number num-str))
     (unless n (error 'parse-number "Invalid Expression"))
     (values n rest1)]))

;; parse-expr: numbers + unary '-' negation
(define (parse-expr chars history)
  (define cs (skip-ws chars))
  (when (empty? cs)
    (error 'parse-expr "Invalid Expression"))
  (define c (first cs))
  (cond
    [(char=? c #\-)
     (define-values (v rest) (parse-expr (rest cs) history))
     (values (- v) rest)]
    [(digit? c)
     (parse-number cs)]
    [else
     (error 'parse-expr "Invalid Expression")]))

;; parse exactly one expression; error if extra non-ws remains
(define (eval-line line history)
  (define chars (string->list line))
  (define-values (v rest) (parse-expr chars history))
  (define rest2 (skip-ws rest))
  (if (empty? rest2)
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
                        (displayln (string-append "DEBUG exn: " (exn-message e))) ;;debugging
                        (print-error "Invalid Expression")
                        (repl history next-id))])
       (let ([v (eval-line line history)])
         (displayln v) ;;debugging
         (print-result next-id v)
         (repl (cons v history) (add1 next-id))))]))

(module+ main
  (repl '() 1))
