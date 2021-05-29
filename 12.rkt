#lang racket

;;------------------------------------------------------------------------------
;; Day 12
;;------------------------------------------------------------------------------

(define input-path "12.txt")

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1)
  (call-with-input-file
    input-path
    (lambda (in)
      (for/fold ([north-south 0]
                 [east-west 0]
                 [facing "E"]
                 #:result (+ (abs north-south) (abs east-west)))
                ([line (in-lines in)])
        (let ([action (substring line 0 1)]
              [value (string->number (substring line 1))])
          (do-action north-south east-west facing action value))))))

(define (do-action north-south east-west facing action value)
  (case action
    [("N") (values (+ north-south value) east-west facing)]
    [("S") (values (- north-south value) east-west facing)]
    [("E") (values north-south (+ east-west value) facing)]
    [("W") (values north-south (- east-west value) facing)]
    [("L" "R") (values north-south east-west (rotate facing action value))]
    [("F") (do-action north-south east-west facing facing value)]))

(define directions #("N" "E" "S" "W"))

(define (rotate facing action value)
  (let ([sign (if (string=? "L" action) - +)])
    (vector-ref directions
                (modulo (+ (vector-memq facing directions)
                           (sign (/ value 90)))
                        4))))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

;      s-ew:  0  s-ns:  0  w-ew: 10  w-ns:  1
; F10  s-ew:100  s-ns: 10  w-ew: 10  w-ns:  1
; N3   s-ew:100  s-ns: 10  w-ew: 10  w-ns:  4
; F7   s-ew:170  s-ns: 38  w-ew: 10  w-ns:  4
; R90  s-ew:170  s-ns: 38  w-ew:  4  w-ns:-10
; F11  s-ew:214  s-ns:-72  w-ew:  4  w-ns:-10

(define (solve-part-2)
  (for/fold ([wayp-ns 1]
             [wayp-ew 10]
             [ship-ns 0]
             [ship-ew 0]
