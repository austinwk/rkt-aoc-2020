#lang racket

;;------------------------------------------------------------------------------
;; Day 11
;;------------------------------------------------------------------------------

(define input-path "11.txt")

(define (get-layout)
  (call-with-input-file
    input-path
    (lambda (in)
      (for/vector ([line (in-lines in)])
        (list->vector (string->list line))))))

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define FLOOR_CHAR #\.)
(define OCCUPIED_CHAR #\#)
(define EMPTY_CHAR #\L)

(define (solve-part-1)
  (let iter ([current (get-layout)])
    (let ([next (next-layout current)])
      (if (equal? current next)
          next
          (iter next)))))

(define (next-layout layout)
  (let* ([num-rows (vector-length layout)]
         [num-cols (vector-length (vector-ref layout 0))]
         [next (make-matrix num-rows num-cols)])
    (for* ([row (in-range num-rows)]
           [col (in-range num-cols)])
      (vector-set! (vector-ref next row) col (next-seat layout row col)))
    next))

(define (make-matrix num-rows num-cols)
  (for/vector ([row (in-range num-rows)])
    (make-vector num-cols)))

(define (next-seat layout row col)
  (let* ([seat (layout-ref layout row col)]
         [surrounding (surrounding-seats layout row col)]
         [num-occupied (count-occupied surrounding)]
         [num-empty (count-empty surrounding)])
    (cond [(floor? seat) FLOOR_CHAR]
          [(and (seat-empty? seat) (= 0 num-occupied)) OCCUPIED_CHAR]
          [(and (seat-occupied? seat) (>= 4 num-occupied)) EMPTY_CHAR]
          [else seat])))
    
(define (floor? char)
  (char=? FLOOR_CHAR char))
  
(define (seat-occupied? char)
  (char=? OCCUPIED_CHAR char))

(define (seat-empty? char)
  (char=? EMPTY_CHAR char))

(define (count-occupied seats)
  (count (lambda (seat) (seat-occupied? seat)) seats))

(define (count-empty seats)
  (count (lambda (seat) (seat-empty? seat)) seats))

(define (surrounding-seats layout row col)
  (list (layout-ref layout (sub1 row)       col)    ; North
        (layout-ref layout (sub1 row) (add1 col))   ; Northeast
        (layout-ref layout       row  (add1 col))   ; East
        (layout-ref layout (add1 row) (add1 col))   ; Southeast
        (layout-ref layout (add1 row)       col)    ; South
        (layout-ref layout (add1 row) (sub1 col))   ; Southwest
        (layout-ref layout       row  (sub1 col))   ; West
        (layout-ref layout (sub1 row) (sub1 col)))) ; Northwest

(define (layout-ref layout row col)
  (let ([num-rows (vector-length layout)]
        [num-cols (vector-length (vector-ref layout 0))])
    (if (and (< -1 row num-rows)
             (< -1 col num-cols))
        (vector-ref (vector-ref layout row) col)
        FLOOR_CHAR))) ; Anything out of bounds is floor

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (error "unimplemented"))
