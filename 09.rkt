#lang racket

;;------------------------------------------------------------------------------
;; Day 9
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1) ;=> 507622668
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
            target)))))

(define (target-in-chunk? chunk target)
  (for/or ([cmb (in-combinations chunk 2)])
    (match-let ([(list m n) cmb])
      (and (not (= m n))
           (= target (+ m n))))))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2) ;=> 76688505
  (define target 507622668)
  (define nums (get-nums))
  (let iter ([l1 nums]
             [l2 (cdr nums)]
             [group '()]
             [acc 0])
    (cond [(= acc target) (+ (apply min group) (apply max group))]
          [(> acc target) (iter (cdr l1) (cddr l1) '() 0)]
          [else (iter l1 (cdr l2) (cons (car l2) group) (+ acc (car l2)))])))

(define (get-nums)
  (call-with-input-file
    "09.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (string->number line)))))
