#lang racket
(provide the within
         define-user-command all-user-commands
         define-walkthrough
         set-find-the! set-find-within! set-restart-game!)

(define find-the #f)
(define find-within #f)

(define (set-find-the! impl)
  (set! find-the impl))

(define (set-find-within! impl)
  (set! find-within impl))

(define-syntax-rule (the type ...)
  (find-the '(type ...)))

(define-syntax-rule (within container type ...)
  (find-within container '(type ...)))

(define commands (vector '()))

(define-syntax-rule (define-user-command command-pattern documentation)
  (vector-set! commands 0
               (cons (list 'command-pattern documentation)
                     (all-user-commands))))

(define (all-user-commands)
  (vector-ref commands 0))

(define restart-game #f)

(define (set-restart-game! p)
  (set! restart-game p))

(define-syntax-rule (define-walkthrough name command ...)
  (define (name)
    (begin
      (display "RESTARTING GAME")
      (newline)
      (restart-game)
      (begin (display 'command)
             (newline)
             command)
      ...
      (display "WALKTHROUGH COMPLETE")
      (newline))))


