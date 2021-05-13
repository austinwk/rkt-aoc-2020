#lang racket

(provide (all-defined-out))

;;------------------------------------------------------------------------------
;; Day 4
;;------------------------------------------------------------------------------

(struct pp-field (key val))

; iyr:2016 hgt:193cm eyr:2029
; byr:1934 hcl:#b6652a pid:901756621
; ecl:brn

(define (get-passports)
  (call-with-input-file
    "04.txt"
    (lambda (in)
      (let iter ([line (read-line in)]
                 [passports '()]
                 [batch '()])
        (cond [(eof-object? line) (reverse passports)]
              [(string=? "" line) (iter (read-line in) (cons batch passports) '())]
              [else (iter (read-line in)
                          passports
                          (append (for/list ([kv (in-list (string-split line))])
                                    (match-let ([(list k v) (string-split kv ":")])
                                      (pp-field k v)))
                                  batch))])))))

(displayln (get-passports))
      

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1)
  (error "unimplemented"))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (error "unimplemented"))

;;------------------------------------------------------------------------------
;; Tests
;;------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (test-eq? "solve-part-1" (solve-part-1) #f)
  (test-eq? "solve-part-2" (solve-part-2) #f))
