#lang racket

;;------------------------------------------------------------------------------
;; Day #
;;------------------------------------------------------------------------------

; light red bags contain 1 bright white bag, 2 muted yellow bags.
; dark orange bags contain 3 bright white bags, 4 muted yellow bags.
; bright white bags contain 1 shiny gold bag.
; muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
; shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
; dark olive bags contain 3 faded blue bags, 4 dotted black bags.
; vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
; faded blue bags contain no other bags.
; dotted black bags contain no other bags.

(define (get-input)
  (call-with-input-file
    "07.txt"
    (lambda (in)
      (define rules (make-hash))
      (for ([line (in-port read-line in)])
        (let* ([tokens (string-split line #rx" bags contain no other bags.| bags contain | bags, | bag, | bags.")]
              [rule (parse-rule tokens)])
          (println rule)
          (hash-set! rules (car rule) (cdr rule)))))))

(define (parse-rule tokens)
  (list (car tokens)
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
