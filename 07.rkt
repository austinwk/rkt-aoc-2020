#lang racket

;;------------------------------------------------------------------------------
;; Day #
;;------------------------------------------------------------------------------

; Question: how to (hash-set! h (values "key" "value")) ?

(define (get-input)
  (call-with-input-file
    "07.txt"
    (lambda (in)
      (define rules (make-hash))
      (for ([line (in-port read-line in)])
        (let*-values ([(tokens) (string-split line #rx" bags contain no other bags.| bags contain | bags, | bag, | bags.")]
                      [(k v) (parse-rule tokens)])
          (hash-set! rules k v)))
      rules)))

(define (parse-rule tokens)
  (values (car tokens)
          (for/list ([token (in-list (cdr tokens))])
            (cons (substring token 2)
                  (string->number (substring token 0 1))))))

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
