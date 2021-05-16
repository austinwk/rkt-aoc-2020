#lang racket

;;------------------------------------------------------------------------------
;; Day 3
;;------------------------------------------------------------------------------

(define (get-input)
  (file->lines "03.txt"))

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1) ;=> 726
  (for/fold ([trees 0])
            ([row (in-range +inf.0)]
             [line (in-list (get-input))])
    (if (char=? #\# (string-ref line (modulo (* row 3) 31)))
        (add1 trees)
        trees)))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2) ;=> 7812180000
  (for/fold ([product 1])
            ([slope (in-list '((1 1) (3 1) (5 1) (7 1) (1 2)))])
    (* product (apply count-trees slope))))
    
(define (count-trees right down)
  (for/fold ([col 0]
             [trees 0]
             #:result trees)
            ([(line row) (in-indexed (get-input))]
             #:when (zero? (modulo row down)))
    (values (+ col right)
            (if (char=? #\# (string-ref line (modulo col 31)))
                (add1 trees)
                trees))))
