#lang racket

;;------------------------------------------------------------------------------
;; Day 11
;;------------------------------------------------------------------------------

; If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
; If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
; Otherwise, the seat's state does not change.
; Floor (.) never changes; seats don't move, and nobody sits on the floor.
; Simulate your seating area by applying the seating rules repeatedly until no seats change state. How many seats end up occupied?

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

(define num-rows 93)
(define num-cols 94)
(define floor-char #\.)
(define occupied-char #\#)
(define empty-char #\L)

(define (solve-part-1)
  (let iter ([current (get-layout)])
    (let ([next (next-layout current)])
      (if (equal? current next)
          next
          (iter next)))))

(define (next-layout layout)
  (define next (make-layout))
  (for* ([row (in-range num-rows)]
         [col (in-range num-cols)])
    (vector-set! (vector-ref next row) col (next-seat layout row col)))
  next)

(define (make-layout)
  (for/vector ([row (in-range num-rows)])
    (make-vector num-cols)))

(define (next-seat layout row col)
  (let* ([current (layout-ref layout row col)]
         [surrounding (surrounding-seats layout row col)]
         [num-occupied (count (lambda (c) (char=? c occupied-char)) surrounding)]
         [num-empty (count (lambda (c) (char=? c empty-char)) surrounding)])
    (cond [(char=? floor-char current) floor-char]
          [(and (char=? empty-char current)
                (= 0 num-occupied))
             occupied-char]
          [(and (char=? occupied-char current)
                (>= 4 num-occupied))
             empty-char]
          [else current])))

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
  (if (and (< -1 row num-rows)
           (< -1 col num-cols))
      (vector-ref (vector-ref layout row) col)
      floor-char)) ; Anything out of bounds is floor

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (error "unimplemented"))
