#lang racket

(provide mark-of-beast)

; Gives every object a unique value:
(define mark-of-beast
  (let* ([index (make-hash)]
         [max   0]
         [next  (lambda ()
                  (set! max (+ max 1))
                  max)])
    (lambda (object)
      (if (hash-has-key? index object)
          (hash-ref index object)
          (begin
            (hash-set! index object (next))
            (mark-of-beast object))))))

