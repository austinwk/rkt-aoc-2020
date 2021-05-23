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

(define (solve-part-1)
  (let iter ([count 0]
             [previous (get-layout)])
    (let ([next (next-layout previous)])
      (if (equal? previous next)
          count
          (iter (add1 count) next)))))

(define layout-rows 93)
(define layout-cols 94)
(define floor-char #\.)
(define occupied-char #\#)
(define empty-char #\L)

(define (next-layout layout)
  (define next-layout (make-blank-layout))
  (for* ([row (in-range layout-rows)]
         [col (in-range layout-cols)])
    (vector-set! (vector-ref next-layout row) col (next-state-at layout row col)))
  next-layout)

(define (make-blank-layout)
  (for/vector ([_ (in-range layout-rows)])
    (make-vector layout-cols)))

(define (next-state-at layout row col)
  (define seat-char (layout-ref layout row col))
  (for/fold ([num-empty 0]
             [num-occupied 0]
             #:result (interpret-seat-state seat-char num-empty num-occupied))
             ;             N        NE      E       SE      S       SW       W         NW
            ([offset '((-1 . 0) (-1 . 1) (0 . 1) (1 . 1) (1 . 0) (1 . -1) (0 . -1) (-1 . -1))])
    (match (layout-ref layout (+ row (car offset)) (+ col (cdr offset)))
      [empty-char (values (add1 num-empty) num-occupied)]
      [occupied-char (values num-empty (add1 num-occupied))]
      [floor-char (values num-empty num-occupied)])))

(define (interpret-seat-state seat-char num-empty num-occupied)
  (cond [(char=? seat-char floor-char) floor-char]
        [(and (char=? seat-char empty-char) (= 0 num-occupied)) occupied-char]
        [(and (char=? seat-char occupied-char) (>= 4 num-occupied)) empty-char]
        [else seat-char]))       

(define (layout-ref layout row col)
  (if (and (> layout-rows row -1)
           (> layout-cols col -1))
      (vector-ref (vector-ref layout row) col)
      empty-char))

; n:  (sub1 row) col
; ne: (sub1 row) (add1 col)
; e:  row        (add1 col)
; se: (add1 row) (add1 col)
; s:  (add1 row) col
; sw: (add1 row) (sub1 col)
; w:  row        (sub1 col)
; nw  (sub1 row) (sub1 col)

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (error "unimplemented"))
