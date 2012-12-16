#lang racket

;; Input Language: 

;; <program> ::= (program <exp> ...)

;; <exp> ::= <var>
;;         | <boolean>
;;         | <number>
;;         | <string>
;;         | (void)
;;         | (<exp> <exp> ...)
;;         | (set! <var> <exp>)
;;         | (if <exp> <exp> <exp>)
;;         | (while <exp> <exp>)
;;         | (begin <exp> ...)
;;         | (<binop> <exp> <exp>)

;; <binop> ::= < | <= | > | >= | > | == | !=

;; Output Language:

;; <exp> ::= (let ([<var> <exp>]) <exp>)
;;         | <aexp>
;;         | <cexp>

;; <aexp> ::= <var>
;;          | <boolean>
;;          | <number>
;;          | <string>
;;          | (void)

;; <cexp> ::= (<aexp> <aexp> ...)
;;          | (set! <var> <exp>)
;;          | (if <aexp> <exp> <exp>)

(define (atomic? exp)
  (match exp
    [(? symbol?)  #t]
    [(? boolean?) #t]
    [(? number?)  #t]
    [(? string?)  #t]
    ['(void)      #t]
    [else         #f]))

(define (normalize-term exp)
  (normalize exp (λ (x) x)))

(define (normalize exp k)
  (match exp
    [`(λ ,params ,body)  
     (k `(λ ,params ,(normalize-term body)))]
    
    [`(let () ,exp)
     (normalize exp k)]
    
    [`(let ([,x ,exp1] . ,clause) ,exp2) 
     (normalize exp1 (λ (aexp1) 
                       `(let ([,x ,aexp1])
                          ,(normalize `(let (,@clause) ,exp2) k))))]    
    
    [`(letrec ([,x ,exp1]) ,exp2)
     (normalize exp1 (λ (aexp1) 
                       `(letrec ([,x ,aexp1])
                          ,(normalize exp2 k))))]
    
    [`(begin ,exp)
     (normalize-name exp (λ (t) (k t)))]
    
    [`(begin ,exp . ,rest)
     (normalize-name exp (λ (t) 
                           `(let ([,(gensym '_) ,t])
                              ,(normalize `(begin ,@rest) k))))]
    
    [`(if ,exp1 ,exp2 ,exp3)
     ; allow predicates that are acceptable to HyTech
     (k `(if ,exp1 ,(normalize-term exp2)
             ,(normalize-term exp3)))]
    [`(while ,exp1 ,exp2)
     (normalize-name exp1 (λ (t)
                            (k `(while ,t ,(normalize-term exp2)))))]
    
    [`(set! ,v ,exp)
     (normalize-name exp (λ (t)
                           `(let ([,(gensym '_) (set! ,v ,t)])
                              ,(k v))))]
    
    [`(call/cc ,exp)
     (normalize-name exp (λ (c)
                           (k `(call/cc ,c))))]
    
    [`(,f . ,args)
     (normalize-name f (λ (f)
                         (normalize-name* args (λ (args)
                                                 (k `(,f . ,args))))))]
    
    [(? atomic?)
     (k exp)]))

(define (normalize-name exp k)
  (normalize exp (λ (exp)
                   (if (atomic? exp)
                       (k exp)
                       (let ([t (gensym)])
                         `(let ([,t ,exp]) ,(k t)))))))

(define (normalize-name* exps k)
  (if (empty? exps)
      (k empty)
      (normalize-name (car exps) (λ (t) 
                                   (normalize-name* (cdr exps) (λ (t*) 
                                                                 (k `(,t . ,t*))))))))

(define (normalize-program program)
  (match program
    [`(program . ,exps)
     (normalize-term `(begin ,@exps))]))

(define input #f)

(match (current-command-line-arguments)
  [(vector filename) (set! input (read filename))]
  [else (set! input (read))])

(pretty-write (normalize-program input))
