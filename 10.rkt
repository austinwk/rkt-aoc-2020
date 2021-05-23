#lang racket

;;------------------------------------------------------------------------------
;; Day 10
;;------------------------------------------------------------------------------

(define input-path "10.txt")

(define (get-adapters)
  (call-with-input-file
    input-path
    (lambda (in)
      (let ([adapters (for/list ([line (in-lines in)])
                        (string->number line))])
        (make-connection adapters)))))

(define (make-connection adapters)
  ((compose1 list->vector
             (lambda (ls) (sort ls <))
             (lambda (ls) (cons 0 ls))
             (lambda (ls) (cons (+ 3 (apply max ls)) ls)))
   adapters))

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1) ;=> 1885
  (let* ([conn (get-adapters)]
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

;;------------------------------------------------------------------------------
;; Part 2
;;
;; Credit: https://github.com/viliampucik/adventofcode/blob/master/2020/10.py
;;------------------------------------------------------------------------------

(define (solve-part-2) ;=> 2024782584832
  (define counts (make-hash))
  (hash-set! counts 0 1)
  (define adapters (vector-drop (get-adapters) 1))
  (for ([a (in-vector adapters)])
    (hash-set! counts a (+ (hash-ref! counts (- a 3) 0)
                           (hash-ref! counts (- a 2) 0)
                           (hash-ref! counts (- a 1) 0))))
  (hash-ref counts (vector-ref adapters (sub1 (vector-length adapters)))))
