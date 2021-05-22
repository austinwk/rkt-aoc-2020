#lang racket

;;------------------------------------------------------------------------------
;; Day 10
;;------------------------------------------------------------------------------

(define input-path "10.txt")

(define (get-adapters)
  (call-with-input-file
    input-path
    (lambda (in)
      (for/list ([line (in-lines in)])
        (string->number line)))))

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1) ;=> 1885
  (let* ([conn (make-connection (get-adapters))]
         [len (vector-length conn)])
    (for/fold ([ones 0]
               [threes 0]
               #:result (* ones threes))
              ([i (in-range (sub1 len))]
               [j (in-range 1 len)])
      (match (- (vector-ref conn j) (vector-ref conn i))
        [1 (values (add1 ones) threes)]
        [2 (values ones threes)]
        [3 (values ones (add1 threes))]))))

(define (make-connection adapters)
  ((compose1 list->vector
             (lambda (ls) (sort ls <))
             (lambda (ls) (cons 0 ls))
             (lambda (ls) (cons (+ 3 (apply max ls)) ls)))
   adapters))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (error "unimplemented"))
