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

(define (solve-part-2)
  (error "unimplemented"))
