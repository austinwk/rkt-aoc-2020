#lang racket

;;------------------------------------------------------------------------------
;; Day 8
;;------------------------------------------------------------------------------

(struct instruction (op arg ex?))

(define (get-instructions)
  (call-with-input-file
    "08.txt"
    (lambda (in)
      (for/vector ([line (in-port read-line in)])
        (instruction (string->symbol (substring line 0 3))
                     (string->number (substring line 4))
                     #f)))))

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1)
  (execute-instruction (get-instructions) 0 0)) ;=> 1200

(define (execute-instruction instructions i acc)
  (let ([inst (vector-ref instructions i)])
    (if (instruction-ex? inst)
        acc
        (begin (vector-set! instructions i (struct-copy instruction inst [ex? #t]))
               (case (instruction-op inst)
                 [(acc) (execute-instruction instructions (add1 i) (+ acc (instruction-arg inst)))]
                 [(jmp) (execute-instruction instructions (+ i (instruction-arg inst)) acc)]
                 [(nop) (execute-instruction instructions (add1 i) acc)])))))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2)
  (error "unimplemented"))
