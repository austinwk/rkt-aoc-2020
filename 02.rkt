#lang racket

;;------------------------------------------------------------------------------
;; Day 2
;;------------------------------------------------------------------------------

(struct pwd (m n c s)
            #:transparent)

(define (get-input)
  (map string->pwd (file->lines "02.txt")))

(define (string->pwd str)
  (match-let* ([(list mn c s) (string-split str)]
               [(list m n) (map string->number (string-split mn "-"))])
    (pwd m n (string-ref c 0) s)))

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1) ;=> 396
  (count valid-pwd-p1? (get-input)))

(define (valid-pwd-p1? p)
  (let ([n (count (lambda (c) (char=? c (pwd-c p)))
                  (string->list (pwd-s p)))])
    (<= (pwd-m p) n (pwd-n p))))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2) ;=> 428
  (count valid-pwd-p2? (get-input)))

(define (valid-pwd-p2? p)
  (let* ([c (pwd-c p)]
         [s (pwd-s p)]
         [a? (char=? c (string-ref s (- (pwd-m p) 1)))]
         [b? (char=? c (string-ref s (- (pwd-n p) 1)))])
    (xor a? b?)))
