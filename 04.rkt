#lang racket

;;------------------------------------------------------------------------------
;; Day 4
;;------------------------------------------------------------------------------

(define input-path "04.txt")

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1) ;=> 202
  (call-with-input-file
    input-path
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

(define (solve-part-2) ;=> 137
  (call-with-input-file
    input-path
    (lambda (in)
      (let iter ([line (read-line in)]
                 [valid 0]
                 [passport '()])
        (cond [(eof-object? line) valid]
              [(string=? "" line) (if (passport-valid? passport)
                                      (iter (read-line in) (add1 valid) '())
                                      (iter (read-line in) valid '()))]
              [else (iter (read-line in)
                          valid
                          (append passport
                                  (for/list ([token (string-split line)])
                                    (string-split token ":"))))])))))

(define field-validators
  (hash "byr" (lambda (v) (string<=? "1920" v "2002"))
        "iyr" (lambda (v) (string<=? "2010" v "2020"))
        "eyr" (lambda (v) (string<=? "2020" v "2030"))
        "hgt" (lambda (v) (case (substring v (- (string-length v) 2)) 
                            [("cm") (string<=? "150" (string-trim v "cm") "193")]
                            [("in") (string<=? "59" (string-trim v "in") "76")]
                            [else #f]))
        "hcl" (lambda (v) (regexp-match? #px"^#[0-9a-f]{6}$" v))
        "ecl" (lambda (v) (member v '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")))
        "pid" (lambda (v) (regexp-match? #px"^[0-9]{9}$" v))))

(define required-fields (hash-keys field-validators))

(define (passport-valid? passport)
  (for/and ([req-fld (in-list required-fields)])
    (let ([pp-fld (assoc req-fld passport)])
      (and pp-fld ((hash-ref field-validators (first pp-fld)) (second pp-fld))))))
