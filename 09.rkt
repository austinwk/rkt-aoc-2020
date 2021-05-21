#lang racket

;;------------------------------------------------------------------------------
;; Day 9
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1)
  (call-with-input-file
    "09.txt"
    (lambda (in)        ; Init with preamble
      (let iter ([chunk (do ([p '() (cons (string->number (read-line in)) p)])
                            ((= 25 (length p)) p))]
                 [target (string->number (read-line in))])
        (if (target-in-chunk? chunk target)
            (iter (cons target (take chunk 24))
                  (string->number (read-line in)))
            target)))))

(define (target-in-chunk? chunk target)
  (for/or ([cmb (in-combinations chunk 2)])
    (match-let ([(list m n) cmb])
      (and (not (= m n))
           (= target (+ m n))))))
;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (error "unimplemented"))
