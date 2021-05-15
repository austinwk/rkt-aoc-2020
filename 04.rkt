#lang racket

(provide (all-defined-out))

;;------------------------------------------------------------------------------
;; Day 4
;;------------------------------------------------------------------------------

; Not used
(struct pp-field (key val))

; Not used
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

(define (solve-part-1)
  (call-with-input-file
    "04.txt"
    (lambda (in)
      (let iter ([line (read-line in)]
                 [valid 0]
                 [batch '()])
        (cond [(eof-object? line) valid]
              [(string=? "" line) (if (for/and ([fld '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")])
                                        (member fld batch))
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

(define fields
  (let ([h (make-hash)])
    (hash-set! h "byr" (lambda (v) (string<=? "1920" v "2002")))
    (hash-set! h "iyr" (lambda (v) (string<=? "2010" v "2020")))
    (hash-set! h "eyr" (lambda (v) (string<=? "2020" v "2030")))
    (hash-set! h "hgt" (lambda (v) (let* ([len (string-length v)]
                                          [unt (substring v (- len 2))])
                                     (case unt
                                       [("cm") (string<=? "150" (substring v 0 (- len 2)) "193")]
                                       [("in") (string<=? "59" (substring v 0 (- len 2)) "76")]
                                       [else #f]))))
    (hash-set! h "hcl" (lambda (v) (regexp-match? #px"^#[0-9a-f]{6}$" v)))
    (hash-set! h "ecl" (lambda (v) (member v '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))))
    (hash-set! h "pid" (lambda (v) (regexp-match? #px"^[0-9]{9}$" v)))
    h))

(define keys (hash-keys fields))

(define (batch-valid? batch)
  (for/and ([key (in-list keys)])
    (let ([kv (assoc key batch)])
      (and kv ((hash-ref fields (first kv)) (second kv))))))

(define (solve-part-2)
  (call-with-input-file
    "04.txt"
    (lambda (in)
      (let iter ([line (read-line in)]
                 [valid 0]
                 [batch '()])
        (cond [(eof-object? line) valid]
              [(string=? "" line) (if (batch-valid? batch)
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
