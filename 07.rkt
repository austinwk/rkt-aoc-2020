#lang racket

;;------------------------------------------------------------------------------
;; Day 7
;;------------------------------------------------------------------------------

(define input-path "07.txt")

(define (get-rules)
  (call-with-input-file
    input-path
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

(define (solve-part-1) ;=> 274
  (let ([rules (get-rules)])
    (for/fold ([count 0])
              ([bag (in-hash-keys rules)])
      (if (holds-gold? rules bag)
          (add1 count)
          count))))

(define (holds-gold? rules bag)
  (let ([sub-bags (hash-ref rules bag)])
        (cond [(null? sub-bags) #f]
              [(assoc "shiny gold" sub-bags) #t]
              [else (for/or ([sub-bag (in-list sub-bags)])
                      (holds-gold? rules (car sub-bag)))])))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2) ;=> 158730
  (count-sub-bags (get-rules) "shiny gold"))

(define (count-sub-bags rules bag)
  (let ([sub-bags (hash-ref rules bag)])
    (if (null? sub-bags)
        0
        (for/sum ([sub-bag (in-list sub-bags)])
          (+ (cdr sub-bag)
             (* (cdr sub-bag)
                (count-sub-bags rules (car sub-bag))))))))
