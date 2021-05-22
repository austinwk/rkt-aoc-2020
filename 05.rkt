#lang racket

;;------------------------------------------------------------------------------
;; Day 5
;;------------------------------------------------------------------------------

(define (seat-id pass)
  (+ (* 8 (seat-row pass)) (seat-col pass)))

(define (seat-row pass)
  (search-seats (substring pass 0 7) 128 #\F))

(define (seat-col pass)
  (search-seats (substring pass 7) 8 #\L))

(define (search-seats instructions num-seats lower-char)
  (for/fold ([seats (range num-seats)]
             #:result (first seats))
            ([char (in-list (string->list instructions))])
    (if (char=? char lower-char)
        (take seats (/ (length seats) 2))
        (drop seats (/ (length seats) 2)))))

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1) ;=> 855
  (call-with-input-file
    "05.txt"
    (lambda (in)
      (for/fold ([highest-seat-id 0])
                ([pass (in-port read-line in)])
        (let ([id (seat-id pass)])
          (if (> id highest-seat-id)
              id
              highest-seat-id))))))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2) ;=> 552
  (let ([pass-ids (all-pass-ids)])
    (for/first ([a (in-list pass-ids)]
                [b (in-list (cdr pass-ids))]
                #:when (not (= (add1 a) b)))
      (add1 a))))

(define (all-pass-ids)
 (call-with-input-file
   "05.txt"
   (lambda (in)
     (for/fold ([passes '()]
                #:result (sort passes <))
               ([pass (in-port read-line in)])
       (cons (seat-id pass) passes)))))
