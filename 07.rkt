#lang racket

;;------------------------------------------------------------------------------
;; Day #
;;------------------------------------------------------------------------------

(define (get-rules)
  (call-with-input-file
    "07.txt"
    (lambda (in)
      (for/fold ([rules (make-hash)])
                ([line (in-port read-line in)])
        (let*-values ([(tokens) (string-split line #rx" bags contain no other bags.| bags contain | bags, | bag, | bags.| bag.")]
                      [(k v) (parse-rule tokens)])
          (hash-set! rules k v)
          rules)))))

(define (parse-rule tokens)
  (values (car tokens)
          (for/list ([token (in-list (cdr tokens))])
            (cons (substring token 2)
                  (string->number (substring token 0 1))))))

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1)
  (let ([rules (get-rules)])
    (for/fold ([count 0])
              ([bag (in-hash-keys rules)])
      (if (holds-gold? rules bag)
          (add1 count)
          count)))) ;=> 274

; Memoize to reduce drill-down
; Example: (hash "light green" #t
;                "dark yellow" #f)
(define holds-gold-cache (make-hash))

(define (holds-gold? rules bag)
  (if (hash-has-key? holds-gold-cache bag)
      (hash-ref holds-gold-cache bag)
      (let ([sub-bags (hash-ref rules bag)])
           (cond [(null? sub-bags)
                    (hash-set! holds-gold-cache bag #f)
                    #f]
                 [(assoc "shiny gold" sub-bags)
                    (hash-set! holds-gold-cache bag #t)
                    #t]
                 [else (for/or ([sub-bag (in-list sub-bags)])
                         (holds-gold? rules (car sub-bag)))]))))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (error "unimplemented"))
