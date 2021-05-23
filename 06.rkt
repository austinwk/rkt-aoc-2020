#lang racket

;;------------------------------------------------------------------------------
;; Day 6
;;------------------------------------------------------------------------------

(define input-path "06.txt")

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1) ;=> 6443
  (call-with-input-file
    input-path
    (lambda (in)
      (let iter ([line (read-line in)]
                 [group '()]
                 [count 0])
        (cond [(eof-object? line) (+ count (count-answers group))]
              [(string=? "" line) (iter (read-line in) '() (+ count (count-answers group)))]
              [else (iter (read-line in)
                          (cons (string->list line) group)
                          count)])))))

(define (count-answers group)
  ((compose1 length
             remove-duplicates
             flatten)
   group))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2) ;=> 3232
  (call-with-input-file
    input-path
    (lambda (in)
      (let iter ([line (read-line in)]
                 [group '()]
                 [count 0])
        (cond [(eof-object? line) (+ count (set-count (apply set-intersect group)))]
              [(string=? "" line) (iter (read-line in)
                                        '()
                                        (+ count (set-count (apply set-intersect group))))]
              [else (iter (read-line in)
                          (cons (list->set (string->list line)) group)
                          count)])))))
