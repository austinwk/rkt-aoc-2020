#lang racket

;;------------------------------------------------------------------------------
;; Day 11
;;------------------------------------------------------------------------------

(define input-path "11.txt")
(define FLOOR_CHAR #\.)
(define OCCUPIED_CHAR #\#)
(define EMPTY_CHAR #\L)

(define (get-layout)
  (call-with-input-file
    input-path
    (lambda (in)
      (for/vector ([line (in-lines in)])
        (list->vector (string->list line))))))

(define (next-layout layout next-seat-proc)
  (let* ([num-rows (vector-length layout)]
         [num-cols (vector-length (vector-ref layout 0))]
         [next (make-matrix num-rows num-cols)])
    (for* ([row (in-range num-rows)]
           [col (in-range num-cols)])
      (vector-set! (vector-ref next row) col (next-seat-proc layout row col)))
    next))

(define (make-matrix num-rows num-cols)
  (for/vector ([row (in-range num-rows)])
    (make-vector num-cols)))

(define (layout-ref layout row col)
  (let ([num-rows (vector-length layout)]
        [num-cols (vector-length (vector-ref layout 0))])
    (if (in-bounds? layout row col)
        (vector-ref (vector-ref layout row) col)
        FLOOR_CHAR))) ; Anything out of bounds is floor

(define (in-bounds? layout row col)
  (let ([num-rows (vector-length layout)]
        [num-cols (vector-length (vector-ref layout 0))])
    (and (< -1 row num-rows)
         (< -1 col num-cols))))

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

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1)
  (let iter ([current (get-layout)])
    (let ([next (next-layout current next-seat-1)])
      (if (equal? current next)
          (for/sum ([row (in-vector next)])
            (count-occupied (vector->list row)))
          (iter next)))))

(define (next-seat-1 layout row col)
  (let* ([seat (layout-ref layout row col)]
         [surrounding (surrounding-seats layout row col)]
         [num-occupied (count-occupied surrounding)]
         [num-empty (count-empty surrounding)])
    (cond [(floor? seat) FLOOR_CHAR]
          [(and (seat-empty? seat) (= 0 num-occupied)) OCCUPIED_CHAR]
          [(and (seat-occupied? seat) (>= num-occupied 4)) EMPTY_CHAR]
          [else seat])))

(define (surrounding-seats layout row col)
  (list (layout-ref layout (sub1 row)       col)    ; North
        (layout-ref layout (sub1 row) (add1 col))   ; Northeast
        (layout-ref layout       row  (add1 col))   ; East
        (layout-ref layout (add1 row) (add1 col))   ; Southeast
        (layout-ref layout (add1 row)       col)    ; South
        (layout-ref layout (add1 row) (sub1 col))   ; Southwest
        (layout-ref layout       row  (sub1 col))   ; West
        (layout-ref layout (sub1 row) (sub1 col)))) ; Northwest

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

; (define (solve-part-2)
;   (let iter ([current (get-layout)])
;     (let ([next (next-layout current next-seat-2)])
;       (if (equal? current next)
;           (for/sum ([row (in-vector next)])
;             (count-occupied (vector->list row)))
;           (iter next)))))

; (define (next-seat-2 layout row col)
;   (let* ([seat (layout-ref layout row col)]
;          [visible (visible-seats layout row col)]
;          [num-occupied (count-occupied visible)]
;          [num-empty (count-empty visible)])
;     (cond [(floor? seat) FLOOR_CHAR]
;           [(and (seat-empty? seat) (= 0 num-occupied)) OCCUPIED_CHAR]
;           [(and (seat-occupied? seat) (>= num-occupied 5)) EMPTY_CHAR]
;           [else seat])))

; (define (visible-seats layout row col)
;   (for/first ))

; (define (look layout row col row-change col-change)
;   (do ([next-row (+ row row-change)]
;        [next-col (+ col col-change)]))
;   (layout-ref layout (+ row row-change) (+ col col-change)))

;;------------------------------------------------------------------------------
;; Tests
;;------------------------------------------------------------------------------

(module+ test
  (require rackunit)
  
  (define layout1 ; Initial layout
    #(#(#\L #\. #\L #\L #\. #\L #\L #\. #\L #\L)
      #(#\L #\L #\L #\L #\L #\L #\L #\. #\L #\L)
      #(#\L #\. #\L #\. #\L #\. #\. #\L #\. #\.)
      #(#\L #\L #\L #\L #\. #\L #\L #\. #\L #\L)
      #(#\L #\. #\L #\L #\. #\L #\L #\. #\L #\L)
      #(#\L #\. #\L #\L #\L #\L #\L #\. #\L #\L)
      #(#\. #\. #\L #\. #\L #\. #\. #\. #\. #\.)
      #(#\L #\L #\L #\L #\L #\L #\L #\L #\L #\L)
      #(#\L #\. #\L #\L #\L #\L #\L #\L #\. #\L)
      #(#\L #\. #\L #\L #\L #\L #\L #\. #\L #\L)))

  (define layout2
    #(#(#\# #\. #\# #\# #\. #\# #\# #\. #\# #\#)
      #(#\# #\# #\# #\# #\# #\# #\# #\. #\# #\#)
      #(#\# #\. #\# #\. #\# #\. #\. #\# #\. #\.)
      #(#\# #\# #\# #\# #\. #\# #\# #\. #\# #\#)
      #(#\# #\. #\# #\# #\. #\# #\# #\. #\# #\#)
      #(#\# #\. #\# #\# #\# #\# #\# #\. #\# #\#)
      #(#\. #\. #\# #\. #\# #\. #\. #\. #\. #\.)
      #(#\# #\# #\# #\# #\# #\# #\# #\# #\# #\#)
      #(#\# #\. #\# #\# #\# #\# #\# #\# #\. #\#)
      #(#\# #\. #\# #\# #\# #\# #\# #\. #\# #\#)))

  (define layout3
    #(#(#\# #\. #\L #\L #\. #\L #\# #\. #\# #\#)
      #(#\# #\L #\L #\L #\L #\L #\L #\. #\L #\#)
      #(#\L #\. #\L #\. #\L #\. #\. #\L #\. #\.)
      #(#\# #\L #\L #\L #\. #\L #\L #\. #\L #\#)
      #(#\# #\. #\L #\L #\. #\L #\L #\. #\L #\L)
      #(#\# #\. #\L #\L #\L #\L #\# #\. #\# #\#)
      #(#\. #\. #\L #\. #\L #\. #\. #\. #\. #\.)
      #(#\# #\L #\L #\L #\L #\L #\L #\L #\L #\#)
      #(#\# #\. #\L #\L #\L #\L #\L #\L #\. #\L)
      #(#\# #\. #\# #\L #\L #\L #\L #\. #\# #\#)))

  (define layout4
    #(#(#\# #\. #\# #\L #\. #\L #\# #\. #\# #\#)
      #(#\# #\L #\L #\L #\# #\L #\L #\. #\L #\#)
      #(#\L #\. #\L #\. #\L #\. #\. #\# #\. #\.)
      #(#\# #\L #\L #\L #\. #\# #\# #\. #\L #\#)
      #(#\# #\. #\L #\L #\. #\L #\L #\. #\L #\L)
      #(#\# #\. #\L #\L #\# #\L #\# #\. #\# #\#)
      #(#\. #\. #\L #\. #\L #\. #\. #\. #\. #\.)
      #(#\# #\L #\# #\L #\L #\L #\L #\# #\L #\#)
      #(#\# #\. #\L #\L #\L #\L #\L #\L #\. #\L)
      #(#\# #\. #\# #\L #\# #\L #\# #\. #\# #\#)))

  (define layout5
    #(#(#\# #\. #\# #\L #\. #\L #\# #\. #\# #\#)
      #(#\# #\L #\L #\L #\# #\L #\L #\. #\L #\#)
      #(#\L #\. #\# #\. #\L #\. #\. #\# #\. #\.)
      #(#\# #\L #\# #\# #\. #\# #\# #\. #\L #\#)
      #(#\# #\. #\# #\L #\. #\L #\L #\. #\L #\L)
      #(#\# #\. #\# #\L #\# #\L #\# #\. #\# #\#)
      #(#\. #\. #\L #\. #\L #\. #\. #\. #\. #\.)
      #(#\# #\L #\# #\L #\# #\# #\L #\# #\L #\#)
      #(#\# #\. #\L #\L #\L #\L #\L #\L #\. #\L)
      #(#\# #\. #\# #\L #\# #\L #\# #\. #\# #\#)))

  (define layout6 ; Same as layout 5
    #(#(#\# #\. #\# #\L #\. #\L #\# #\. #\# #\#)
      #(#\# #\L #\L #\L #\# #\L #\L #\. #\L #\#)
      #(#\L #\. #\# #\. #\L #\. #\. #\# #\. #\.)
      #(#\# #\L #\# #\# #\. #\# #\# #\. #\L #\#)
      #(#\# #\. #\# #\L #\. #\L #\L #\. #\L #\L)
      #(#\# #\. #\# #\L #\# #\L #\# #\. #\# #\#)
      #(#\. #\. #\L #\. #\L #\. #\. #\. #\. #\.)
      #(#\# #\L #\# #\L #\# #\# #\L #\# #\L #\#)
      #(#\# #\. #\L #\L #\L #\L #\L #\L #\. #\L)
      #(#\# #\. #\# #\L #\# #\L #\# #\. #\# #\#)))

  (test-case "next-seat-1"
    (check-eq? (next-seat-1 layout1 0 0)
               (layout-ref layout2 0 0))
    (check-eq? (next-seat-1 layout2 0 0)
               (layout-ref layout3 0 0))
    (check-eq? (next-seat-1 layout2 1 2)
               (layout-ref layout3 1 2)))

  (test-case "count-occupied"
    (check-eq? (count-occupied '(#\. #\L #\# #\. #\L #\# #\. #\#)) 3)
    (check-eq? (count-occupied '(#\. #\L #\. #\. #\L #\L #\. #\L)) 0)
    (check-eq? (count-occupied '(#\# #\# #\# #\. #\L #\# #\. #\L)) 4))

  (test-case "count-empty"
    (check-eq? (count-empty '(#\. #\L #\# #\. #\L #\# #\. #\#)) 2)
    (check-eq? (count-empty '(#\. #\. #\. #\. #\. #\# #\. #\#)) 0)
    (check-eq? (count-empty '(#\L #\# #\L #\. #\L #\# #\. #\L)) 4))

  (test-case "surrounding-seats"
    (check-equal? (surrounding-seats layout3 5 6)
                  '(#\L #\. #\. #\. #\. #\. #\L #\L))
    (check-equal? (surrounding-seats layout2 0 0)
                  '(#\. #\. #\. #\# #\# #\. #\. #\.))))
