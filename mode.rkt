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
                        (repl history next-id))])
       (define v (eval-line line history))   ; <- we’ll implement this
       (print-result next-id v)
       (repl (cons v history) (add1 next-id)))]))

;; ----------------------------
(define (eval-line line history)
  (error 'eval-line "not implemented"))

(module+ main
  (repl '() 1))
