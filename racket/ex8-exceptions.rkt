#lang racket

;; EXCEPTIONS ARE ALREADY IMPLEMENTED IN RACKET
;; but it's good to know how to do it ourselves

(define *handlers* '()) ; stack data struct filled while running with procedures to handle
; *...* (called earmuffs) is a convention for mutable globals

(define (push-handler proc)
  (set! *handlers* (cons proc *handlers*)))

(define (pop-handler)
  (let ([head (car *handlers*)])
    (set! *handlers* (cdr *handlers*))
    head))

(define (throw x)
  (if (pair? *handlers*) ; = (not (empty? *handlers*))
      ((pop-handler) x) ; calling the (procedure) return of 'pop-handler' (i.e. the handler on top of stack) on 'x'
      (apply error x)))

; example:
; (push-handler displayln)
; (throw 5) ; => 5
; (throw 5) ; => error

(define-syntax try
  (syntax-rules (catch)
    [(_ expr ...
        (catch exception-id exception-body ...))
     (call/cc (λ (exit)
                ; install the handler
                (push-handler (λ (x) ; 'x' is the arg of 'throw'
                                (if (equal? x exception-id) ; check if the name of the argument passed by throw is the one of this handler (the TOS)
                                    (exit
                                     (begin exception-body ...)) ; run the exception body
                                    ; else re-throw
                                    (throw x))))
                (let ([res (begin expr ...)])
                  (pop-handler) ; pop the handler because there are no issues so we can eliminate the TOS of the handlers
                  res)))])) ; simply return the result of expr

(define (foo)
  (displayln "Foo")
  (throw "bad-foo"))

(try
 (displayln "Before foo")
 (foo)
 (display "After foo") ; unreached code becaue foo throws an exception
 (catch "bad-foo"
        ; handle the exception here
        (displayln "I caught a throw.")
        #f))
; if we try to catch an unregistered exception, we'll have an error: contract violation