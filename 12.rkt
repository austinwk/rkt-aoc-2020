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
      (for/fold ([north-south 0] ; North:+, South:-
                 [east-west 0]   ; East:+, West:-
                 [facing 90]     ; North:0*, East:90*, South:180*, West:270*
                 #:result (+ (abs north-south) (abs east-west)))
                ([line (in-lines in)])
        (let ([action (substring line 0 1)]
              [value (string->number (substring line 1))])
              (printf "~a, ~a, ~a\n" action value facing)
          (do-action north-south east-west facing action value))))))

(define (do-action north-south east-west facing action value)
  (case action
    [("N") (values (+ north-south value) east-west facing)]
    [("S") (values (- north-south value) east-west facing)]
    [("E") (values north-south (+ east-west value) facing)]
    [("W") (values north-south (- east-west value) facing)]
    [("L" "R") (values north-south east-west (rotate facing action value))]
    [("F") (do-action north-south east-west facing (degrees->action facing) value)]))

(define (rotate facing action value)
  (let* ([change (if (string=? "S" action) (- value) value)]
         [degrees (abs (+ facing change))])
    (if (> degrees 270)
        (- 360 degrees)
        degrees)))

(define (degrees->action degrees)
  (case degrees
    [(0)   "N"]
    [(90)  "E"]
    [(180) "S"]
    [(270) "W"]))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (error "unimplemented"))
