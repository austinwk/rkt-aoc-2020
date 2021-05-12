#lang racket

(require net/http-client)

(define (get-session-id)
  (with-input-from-file ".session-id"
                        (lambda ()
                          (read-line))))

(define (get-input)
  (let-values ([(status headers in)
                (http-sendrecv "adventofcode.com"
                               "/2020/day/4/input"
                               #:ssl? #t
                               #:headers (list (format "Cookie:	session=~a" (get-session-id))))])
    (displayln (port->string in))))

(displayln (get-input))
