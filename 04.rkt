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

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

; Get a line
; Dice it up
; Gather a batch
; Batch valid?

(define (solve-part-1)
  (call-with-input-file
    "04.txt"
    (lambda (in)
      (let iter ([line (read-line in)]
                 [valid 0]
                 [batch '()])
        (cond [(eof-object? line) valid]
              [(string=? "" line) (if (for/and ([field '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")])
                                        (member field batch))
                                      (iter (read-line in) (add1 valid) '())
                                      (iter (read-line in) valid '()))]
              [else (iter (read-line in)
                          valid
                          (append batch
                                  (for/list ([token (string-split line)])
                                    (substring token 0 3))))])))))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define fields #hash(("byr" . (lambda (v) (string>=? "1920" v "2002")))
                     ("iyr" . (lambda (v) (string>=? "2010" v "2020")))
                     ("eyr" . (lambda (v) (string>=? "2020" v "2030")))
                     ("hgt" . (lambda (v) (let* ([len (string-length v)]
                                                 [num (substring v 0 (- len 2))]
                                                 [unt (substring v (- len 2))])
                                            (case unt
                                              [("cm") (string>=? "150" num "193")]
                                              [("in") (string>=? "59" num "76")]
                                              [else #f]))))))

(define (solve-part-2)
  (call-with-input-file
    "04.txt"
    (lambda (in)
      (let iter ([line (read-line in)]
                 [valid 0]
                 [batch '()])
        (cond [(eof-object? line) valid]
              [(string=? "" line) (if (for/and ([field '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")])
                                        (let ([kv (assoc field batch)])
                                          (and kv (case ))))
                                      (iter (read-line in) (add1 valid) '())
                                      (iter (read-line in) valid '()))]
              [else (iter (read-line in)
                          valid
                          (append batch
                                  (for/list ([token (string-split line)])
                                    (string-split token ":"))))])))))

;;------------------------------------------------------------------------------
;; Tests
;;------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (test-eq? "solve-part-1" (solve-part-1) 202)
  (test-eq? "solve-part-2" (solve-part-2) 137))
