#lang racket

(require net/http-client)

(define (get-session-id)
  (call-with-input-file ".session-id"
                             (lambda (port)
                               (read-line port))))

(define (get-input day)
  (let-values ([(status headers in)
                (http-sendrecv "adventofcode.com"
                               (format "/2020/day/~a/input" day)
                               #:ssl? #t
                               #:headers (list (format "Cookie:	session=~a" (get-session-id))))])
    (port->string in)))

(define (write-input day data)
  (call-with-output-file (string-append (~a day
                                            #:min-width 2
                                            #:align 'right
                                            #:left-pad-string "0")
                                        ".txt")
                           (lambda (port) (display data port))))

(module+ main
  (let ([day (vector-ref (current-command-line-arguments) 0)])
  (write-input day (get-input day))))
