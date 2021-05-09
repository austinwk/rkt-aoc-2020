#lang racket

(provide (all-defined-out))

;;------------------------------------------------------------------------------
;; Day 2
;;------------------------------------------------------------------------------

(struct pwd (m n c s)
            #:transparent)

(define (string->pwd string)
  (let ([tokens (rest (regexp-match #rx"^([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)$" string))])
    (pwd (string->number (first tokens))
         (string->number (second tokens))
         (string-ref (third tokens) 0)
         (fourth tokens))))

(define (get-input)
  (map string->pwd (file->lines "02.txt")))

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (valid-pwd-p1? p)
  (let ([n (count (lambda (c) (char=? c (pwd-c p)))
                  (string->list (pwd-s p)))])
    (and (>= n (pwd-m p))
         (<= n (pwd-n p)))))

(define (solve-part-1)
  (count valid-pwd-p1? (get-input)))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (valid-pwd-p2? p)
  (let* ([c (pwd-c p)]
         [s (pwd-s p)]
         [a? (char=? c (string-ref s (- (pwd-m p) 1)))]
         [b? (char=? c (string-ref s (- (pwd-n p) 1)))])
    (xor a? b?)))

(define (solve-part-2)
  (count valid-pwd-p2? (get-input)))

;;------------------------------------------------------------------------------
;; Tests
;;------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  
  (test-eq? "solve-part-1" (solve-part-1) 396)
  (test-eq? "solve-part-2" (solve-part-2) 428))
