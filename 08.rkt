#lang racket

;;------------------------------------------------------------------------------
;; Day 8
;;------------------------------------------------------------------------------

(define input-path "08.txt")

(struct instruction (op arg ex?))

(define (get-instructions)
  (call-with-input-file
    input-path
    (lambda (in)
      (for/vector ([line (in-port read-line in)])
        (instruction (string->symbol (substring line 0 3))
                     (string->number (substring line 4))
                     #f)))))

;;------------------------------------------------------------------------------
;; Part 1
;;------------------------------------------------------------------------------

(define (solve-part-1) ;=> 1200
  (execute-1 (get-instructions) 0 0))

(define (execute-1 instructions i acc)
  (let ([inst (vector-ref instructions i)])
    (if (instruction-ex? inst)
        acc
        (begin (vector-set! instructions i (struct-copy instruction inst [ex? #t]))
               (match (instruction-op inst)
                 ['acc (execute-1 instructions (add1 i) (+ acc (instruction-arg inst)))]
                 ['jmp (execute-1 instructions (+ i (instruction-arg inst)) acc)]
                 ['nop (execute-1 instructions (add1 i) acc)])))))

;;------------------------------------------------------------------------------
;; Part 2
;;------------------------------------------------------------------------------

(define (solve-part-2) ;=> 1023
  (repair-and-execute (get-instructions)))

(define (repair-and-execute instructions)
  (let ([tgt-i (vector-length instructions)])
    (for/or ([(inst i) (in-indexed instructions)])
      (case (instruction-op inst)
        [(jmp nop) (execute-2 (copy-and-swap instructions i) tgt-i 0 0)]
        [else #f]))))

(define (copy-and-swap instructions i)
  (let* ([copy (vector-copy instructions)]
         [inst (vector-ref copy i)]
         [new-op (if (eq? 'jmp (instruction-op inst)) 'nop 'jmp)])
    (vector-set! copy i (struct-copy instruction inst [op new-op]))
    copy))

(define (execute-2 instructions tgt-i i acc)
  (if (= i tgt-i)
      acc
      (let ([inst (vector-ref instructions i)])
        (if (instruction-ex? inst)
            #f
            (begin (vector-set! instructions i (struct-copy instruction inst [ex? #t]))
                   (match (instruction-op inst)
                     ['acc (execute-2 instructions tgt-i (add1 i) (+ acc (instruction-arg inst)))]
                     ['jmp (execute-2 instructions tgt-i (+ i (instruction-arg inst)) acc)]
                     ['nop (execute-2 instructions tgt-i (add1 i) acc)]))))))
