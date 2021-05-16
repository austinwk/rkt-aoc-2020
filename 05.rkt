#lang racket

;;------------------------------------------------------------------------------
;; Day 5
;;------------------------------------------------------------------------------

(define (get-input)
  (error "unimplemented"))

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1)
  (call-with-input-file
    "05.txt"
    (lambda (in)
      (for/fold ([highest-seat-id 0])
                ([pass (in-port read-line in)])
        (let ([id (seat-id pass)])
          (if (> id highest-seat-id)
              id
              highest-seat-id)))))) ;=> 855

(define (seat-id pass)
  (+ (* 8 (seat-row pass)) (seat-col pass)))

(define (seat-row pass)
  (for/fold ([seats (range 128)]
             #:result (first seats))
            ([chr (in-list (string->list (substring pass 0 6)))])
    (if (char=? chr #\F)
        (take seats (/ (length seats) 2))
        (drop seats (/ (length seats) 2)))))

(define (seat-col pass)
  (for/fold ([seats (range 8)]
             #:result (first seats))
            ([chr (in-list (string->list (substring pass 7)))])
    (if (char=? chr #\L)
        (take seats (/ (length seats) 2))
        (drop seats (/ (length seats) 2)))))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (error "unimplemented"))
