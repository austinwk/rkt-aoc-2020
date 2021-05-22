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
    (lambda (in)
      (define (next-num) (string->number (read-line in)))
      (let iter ([chunk (do ([p '() (cons (next-num) p)]) ; Init chunk with preamble
                            ((= 25 (length p)) p))]
                 [target (next-num)])
        (if (target-in-chunk? chunk target)
            (iter (cons target (take chunk 24))
                  (next-num))
            target))))) ;=> 507622668

(define (target-in-chunk? chunk target)
  (for/or ([cmb (in-combinations chunk 2)])
    (match-let ([(list m n) cmb])
      (and (not (= m n))
           (= target (+ m n))))))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (define target 507622668)
  (define nums (get-nums))
  (for/fold ([all nums]
             [others (cdr nums)]
             [acc 0]
             [group '()]
             #:result (+ (apply min group) (apply max group)))
            ([_ (in-range +inf.0)])
    (= acc target)
    (if (> acc target)
        (values (cdr all) (cddr all) 0 '())
        (values all (cdr others) (+ acc (car others)) (cons (car others) group)))))

(define (get-nums)
  (call-with-input-file
    "09.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (string->number line)))))
