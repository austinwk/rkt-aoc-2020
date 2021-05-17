#lang racket

;;------------------------------------------------------------------------------
;; Day 6
;;------------------------------------------------------------------------------

(require threading)

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1)
  (call-with-input-file
    "06.txt"
    (lambda (in)
      (let iter ([line (read-line in)]
                 [group '()]
                 [count 0])
        (cond [(eof-object? line) (+ count (count-answers group))]
              [(string=? "" line) (iter (read-line in) '() (+ count (count-answers group)))]
              [else (iter (read-line in)
                          (cons (string->list line) group)
                          count)]))))) ;=> 6443

(define (count-answers group)
  (~>> group
       flatten
       remove-duplicates
       length))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (error "unimplemented"))
