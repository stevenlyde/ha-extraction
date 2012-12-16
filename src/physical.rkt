#lang racket

(provide physical-effecting-function?
         physical-input-function?
         physical-atomic?
         physical-atomic-eval)


(define abstract-void (set))
(define abstract-number (set 'number))


; physical functions paired with the abstract value of their return value

(define physical-effecting-functions
  (hash
   'heat abstract-void
   'insertRod1 abstract-void
   'insertRod2 abstract-void
   'removeRod1 abstract-void
   'removeRod2 abstract-void))

(define physical-input-functions
  (hash
   'readTemp abstract-number
   'timeRod1Extracted abstract-number
   'timeRod2Extracted abstract-number))



(define (physical-effecting-function? exp)
  (match exp
    [`(,(? (curry hash-has-key? physical-effecting-functions)) ,args ...) #t]
    [else #f]))

(define (physical-input-function? exp)
  (match exp
    [`(,(? (curry hash-has-key? physical-input-functions)) ,args ...) #t]
    [else #f]))

(define (physical-atomic? exp)
  (or (physical-effecting-function? exp)
      (physical-input-function? exp)))

(define (physical-atomic-eval exp)
  (match exp
    [`(,f ,args ...)
     (cond
       [(physical-effecting-function? exp) (hash-ref physical-effecting-functions f)]
       [(physical-input-function? exp) (hash-ref physical-input-functions f)])]))
