#lang racket

(provide (all-defined-out))

;;------------------------------------------------------------------------------
;; Day 3
;;
;; `in-lines` could be used in place of `in-list`, but that would require
;; closing the port after the procedure ends. `in-list` is simpler.
;;------------------------------------------------------------------------------

(define (get-input)
  (file->lines "03.txt"))

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1)
  (for/fold ([trees 0])
            ([row (in-range +inf.0)]
             [line (in-list (get-input))])
    (if (char=? #\# (string-ref line (modulo (* row 3) 31)))
        (add1 trees)
        trees)))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

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

(define (solve-part-2)
  (for/fold ([product 1])
            ([slope (in-list '((1 1) (3 1) (5 1) (7 1) (1 2)))])
    (* product (apply count-trees slope))))

;;------------------------------------------------------------------------------
;; Tests
;;------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (test-eq? "solve-part-1" (solve-part-1) 276)
  (test-eq? "solve-part-2" (solve-part-2) 7812180000))
