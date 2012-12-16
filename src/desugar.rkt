#lang racket

;; Input Language: 

;; <program> ::= (program <statement> ...)

;; <statement> ::= <expression> | <command>

;; <expression> ::= <simple_expr> | <assign_expr>

;; <assign_expr> ::= (= <assign_lhs> <expression>)

;; <assign_lhs> ::= <simple_expr>

;; <simple_expr> ::= <colon_expr> | (<simple_op> <simple_expr> <simple_expr>)
;; <simple_op> ::= < | <= | > | >= | > | == | != 

;; <colon_expr> ::= <prefix_expr>
;; <prefix_expr> ::= <postfix_expr>

;; <postfix_expr> ::= <primary_expr> | (index <postfix_expr> <expression> ...)

;; <primary_expr> ::= <indentifier> | <constant>

;; <constant> ::= true | false | <number>

;; <command> :: <select_command> | <loop_command>

;; <loop_command> ::= (while <expression> <statement> ...)

;; <select_command> ::= <if_command>

;; <if_command> ::= (cond (<expression> <statement> ...) ...+ [(else <statement> ...)])


;; Output Language: 

;; <program> ::= (program <exp> ...)

;; <exp> ::= <var>
;;         | <boolean>
;;         | <number>
;;         | <string>
;;         | (void)
;;         | (<exp> <exp> ...)
;;         | (set! <var> <exp>)
;;         | (if <exp> <exp> <exp>)
;;         | (call/cc <exp>)
;;         | (begin <exp> ...)
;;         | (<binop> <exp> <exp>)
;;         | (let ((<var> <exp>)) <exp>)
;;         | (λ (<var> ...) <exp>)

;; <binop> ::= < | <= | > | >= | > | == | !=

(define (binop? op)
  (and  (memq op '(< <= > >= == !=)) #t))

(define (transform stmt)
  (match stmt
    ['true  #t]
    ['false #f]
    
    [(? number?) stmt]    
    [(? string?) stmt]
    [(? symbol?) stmt]
    
    [`(= ,exp1 ,exp2)
     `(set! ,(transform exp1) ,(transform exp2))]
    
    [`(cond)                   `(void)]
    [`(cond (else ,body ...))  `(begin ,@(map transform body))]
    [`(cond (,test ,body ...)) `(if ,(transform test) (begin ,@(map transform body)) (void))]
    
    [`(cond (,test ,body ...) ,rest ...)
     `(if ,(transform test) (begin ,@(map transform body)) ,(transform `(cond ,@rest)))]
    
    [`(while ,exp ,body ...)
     `(call/cc (λ (break)
                 ,(transform `(letrec ([loop (λ () (if ,(transform exp) (begin ,@(map transform body) (loop)) (void)))]) (loop)))))]
    
    [`(letrec ((,vs ,es) ...) . ,body)
     `(let ,(for/list ([v vs])
              (list v '(void)))
        (begin
          ,@(map (λ (v e)
                   `(set! ,v ,e))
                 vs es)
          ,@body))]
    
    [`(index ,exp ,exps ...)
     `(,(transform exp) ,@(map transform exps))]
    
    [`(,(? binop? op) ,exp1 ,exp2)
     `(,op ,(transform exp1) ,(transform exp2))]
    
    ['(break) '(break)]))


(define (mutable stmt vars)
  (match stmt
    [`(= ,exp1 ,exp2) (mutable exp2 (set-add vars exp1))]
    [`(cond (,exps ...) ...) (foldl (λ (exps vars) (foldl mutable vars exps)) vars exps)]
    [`(while ,exps ...) (foldl mutable vars exps)]
    [`(index ,exps ...) (foldl mutable vars exps)]
    [`(< ,exps ...)     (foldl mutable vars exps)]
    [else vars]))

(define (mutable* stmts)
  (foldl mutable (set) stmts))

(define (bind vars exp)
  (for/fold ([exp exp])
    ([var vars])
    `(let ([,var (void)]) ,exp)))

(define (transform-program program)
  ; <program> ::= (program <statement> ...)
  (match program
    [`(program . ,stmts)
     `(program ,(bind (mutable* stmts) `(begin ,@(map transform stmts))))]
    [else (error (format "not a program: ~s~n" program))]))

(define input #f)

(match (current-command-line-arguments)
  [(vector filename) (set! input (read filename))]
  [else (set! input (read))])

(pretty-write (transform-program input))
