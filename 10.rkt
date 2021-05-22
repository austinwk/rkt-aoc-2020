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
;;
;; Credit: https://github.com/viliampucik/adventofcode/blob/master/2020/10.py
;;------------------------------------------------------------------------------

  (define (solve-part-2)
    (for/sum ([perm (in-permutations (get-adapters))])
      (if (valid-chain? perm) 1 0)))

  (define (valid-chain? chain)
    (and (<= (first chain) 3)
         (>= (last chain) 149)            ; Known max
         (let ([v (list->vector chain)])
           (for/and ([i (in-range 92)]
                     [j (in-range 1 93)]) ; Known length
             (<= (abs (- (vector-ref v i)
                         (vector-ref v j)))
                 3)))))
