#lang racket

(require "unique.rkt")
(require "physical.rkt")

;; <exp> ::= (let ([<var> <exp>]) <exp>)
;;         | <aexp>
;;         | <cexp>

;; <aexp> ::= <var>
;;          | <boolean>
;;          | <number>
;;          | <string>
;;          | (void)

;; <cexp> ::= (<aexp> <aexp> ...)
;;          | (set! <var> <aexp>)
;;          | (if <aexp> <exp> <exp>)
;;          | (while <aexp> <exp>)

(define (new-region) (gensym 's))

(define (make-regions s)
  (hash-set (hash) s (new-region)))

(define (invert-test test)
  (match test
    [(? boolean?)   (not test)]
    [`(< ,e1 ,e2)  `(>= ,e1 ,e2)]
    [`(> ,e1 ,e2)  `(<= ,e1 ,e2)]
    [`(<= ,e1 ,e2) `(> ,e1 ,e2)]
    [`(>= ,e1 ,e2) `(< ,e1 ,e2)]
    [`(== ,e1 ,e2) `(!= ,e1 ,e2)]
    [`(!= ,e1 ,e2) `(== ,e1 ,e2)]))

(define (exp->symbol e)
  (string->symbol
   (let* ([e (format "~a" e)]
          [e (regexp-replace* "#" e "")]
          [e (regexp-replace* " " e "_")]
          [e (regexp-replace* #rx"[()]" e "")])
     e)))

(define (state-name s)
  (define exp (state-exp s))
  (if (physical-effecting-function? exp)
      (exp->symbol exp)
      #f))

(define (update-region-names regions)
  (define new-names
    (for/fold ([new-names (hash)])
      ([(s r) regions])
      (define name (state-name s))
      (if name
          (hash-set new-names r (exp->symbol (state-exp s)))
          new-names)))
  (for/fold ([new-regions regions])
    ([(k v) regions])
    (if (hash-has-key? new-names v)
        (hash-set new-regions k (hash-ref new-names v))
        new-regions)))

(define (get-vars regions)
  (set->list 
   (apply set
          (for/fold ([vars empty]) ([(s r) regions])
            (define exp (state-exp s))
            (match exp
              [`(physical ,v) (cons v vars)]
              [else vars])))))

(define (reassign cur-region new-region regions states store)
  (match states
    [(list) regions]
    [(list-rest st todo)
     (define-values (succs new-store) (next st store))
     (define new-todo (append (filter (λ (st) (equal? cur-region (hash-ref regions st #f))) succs) todo))
     (reassign cur-region new-region (hash-set regions st new-region) new-todo store)]))

(define (assign-regions regions st succs store)
  (match succs
    [(list) regions]
    [(list ns)
     (if (hash-has-key? regions ns)
         (reassign (hash-ref regions ns) (new-region) regions (list ns) store)
         (hash-set regions ns (hash-ref regions st)))]
    [else
     (for/fold ([regions regions]) ([ns succs])
       (hash-set regions ns (new-region)))]))

(define (make-transitions regions region states store)
  (set->list
   (for/fold ([transitions (set)]) ([s states])
     (define-values (next-states new-store) (next s store))
     (match (state-exp s)
       [`(if ,test ,true-exp ,false-exp)
        (for/fold ([transitions transitions])
          ([next-state next-states])
          (if ((length next-states) . > . 1)
              (if (equal? (state-exp next-state) true-exp)
                  (set-add transitions `(when ,test (goto ,(hash-ref regions (car next-states)))))
                  (set-add transitions `(when ,(invert-test test) (goto ,(hash-ref regions (cadr next-states))))))
              transitions))]
       [else 
        (for/fold ([transitions transitions])
          ([n next-states] #:when (and (hash-has-key? regions n)
                                       (not (eq? region (hash-ref regions n))))) 
          (define next-region (hash-ref regions n))
          (set-add transitions `(when True (goto ,next-region))))]))))


(define (print-hybrid-automata i r store)
  (define regions (update-region-names r))
  (define region-to-states (invert-hash regions))
  (define locs
    (for/list ([(r states) region-to-states])
      `(loc ,r (while True) (wait ,@(get-flow states)) ,@(add-updates (make-transitions regions r states store)
                                                                      (get-updates states)))))
  (pretty-write
   `((var ,@(map (λ (v) `(,v analog)) (get-vars regions)))
     (automaton a1
                (initially ,(hash-ref regions i))
                (synclabs)
                ,@locs))))

(define (add-updates trans updates)
  (if (empty? updates)
      trans
      (map (λ (t)
             (match t
               [`(when ,pred ,goto) `(when ,pred (do ,@updates) ,goto)])) 
           trans)))

(define (get-flow states)
  (for/fold ([flow empty])
    ([s states])
    (match (state-exp s)
      [`(assert_cps ,p) (cons (read (open-input-string p)) flow)]
      [else flow])))

(define (get-updates states)
  (for/fold ([updates empty])
    ([s states])
    (match (state-exp s)
      [`(set_physical ,p) (cons (read (open-input-string p)) updates)]
      [else updates])))

(define (invert-hash h)
  (for/fold ([n (hash)])
    ([(k v) h])
    (hash-update n v (λ (s) (cons k s)) empty)))

;; Helpers
(define empty-set (set))

(define (exp->label exp) exp)

; map-set : (a -> b) (set a) -> (set b)
(define (map-set f s)
  (for/fold ([ns empty-set])
    ([e (in-set s)])
    (set-add ns (f e))))

; take* is like take but allows n to be larger than (length l)
(define (take* l n)
  (for/list ([e (in-list l)]
             [i (in-naturals)]
             #:when (i . < . n))
    e))

;; Abstract state-space.

;; state ::= (state exp env addr time)
(struct state (exp env kaddr time) #:transparent)

;; env = hash[var,addr]
;; A addr environment maps variables to addresses.
(define empty-env (make-immutable-hasheq empty))

; env-lookup : env var -> addr
(define env-lookup hash-ref)

; env-extend : env var addr -> env
(define env-extend hash-set)

; env-extend* : env list[var] list[addr] -> env
(define (env-extend* env vars addrs)
  (for/fold ([env env])
    ([v (in-list vars)]
     [a (in-list addrs)])
    (env-extend env v a)))  

;; store = hash[addr,d]
;; A store (or a heap/memory) maps address to denotable values.
(define empty-store (make-immutable-hash empty))

; store-lookup : store addr -> d
(define (store-lookup s a)
  (hash-ref s a empty-set))

; store-update : store addr d -> store
(define (store-update store addr value)
  (hash-update store addr 
               (lambda (d) (set-union d value))
               empty-set))

; store-update* : store list[addr] list[d] -> store
(define (store-update* store addrs values)
  (for/fold ([store store])
    ([a (in-list addrs)]
     [v (in-list values)])
    (store-update store a v)))

; store-join : store store -> store
(define (store-join store1 store2)
  (for/fold ([new-store store1])
    ([(k v) (in-hash store2)])
    (store-update new-store k v)))

;; clo ::= (make-closure <lambda> <env>)
;; Closures pair a lambda term with a addr environment that
;; determinse the value of its free variables.
(struct closure (lam env) #:prefab)

;; addr ::= (make-addr <var> <time>)
;; A addr is minted each time a variable gets bound to a value.
(struct addr (var time) #:prefab)

;; time = (listof label)
;; In k-CFA, time is a bounded memory of program history.
;; In particular, it is the last k call sites through which
;; the program has traversed.
(define time-zero empty)

;; Continuations

(struct halt () #:prefab)
(struct letk (var body env kaddr) #:prefab)
(struct kontp (kaddr) #:prefab)

;; k-CFA parameters

;; Change these to alter the behavior of the analysis.

; k : natural
(define k 3)

; tick : exp time -> time
(define (tick exp time)
  (take* (list* (exp->label exp) time) k))

; alloc : var -> time -> addr
(define ((alloc time) var)
  (addr var time))

;; k-CFA abstract interpreter                 

; lambda? : exp -> boolean
(define (lambda? exp)
  (match exp
    [`(,(or 'lambda 'λ) . ,_) #t]
    [_ #f]))

; atomic? exp -> boolean
(define (atomic? exp)
  (match exp
    [(? lambda?) #t]
    [(? symbol?) #t]
    [(? number?) #t]
    [(? boolean?) #t]
    [(? string?) #t]
    [(or '+ '- '* '/ '=) #t]
    ['(void) #t]
    [(? physical-atomic?) #t]
    [`(physical ,var) #t]
    [`(assert_cps ,e) #t]
    [`(set_physical ,e) #t]
    [`(,(? binop?) ,v1 ,v2) #t]
    [else #f]))

; atomic-eval : env store -> exp -> d
(define (test-eval env store exp)
  ((atomic-eval env store) exp))

(define (binop? op)
  (and (memq op '(< > <= >= == !=)) #t))

; atomic-eval : env store -> exp -> d
(define ((atomic-eval env store) exp)
  (match exp
    [(? boolean?) (set exp)]
    [(? number?) (set 'number)]
    ['(void) (set)]
    [(? physical-atomic?) (physical-atomic-eval exp)]
    [`(physical ,var) (set)]
    [`(assert_cps ,e) (set)]
    [`(set_physical ,e) (set)]
    [`(,(? binop? op) ,v1 ,v2) 
     (set #t #f)]
    [(? symbol? var)
     (store-lookup store (env-lookup env var))]
    [(? lambda? lam)
     (set (closure lam env))]))

; next : state store -> (values list[state] store)
(define (next s store)
  (match-define (state exp env kaddr time) s)
  (define time* (tick exp time))
  
  (match exp
    
    [`(let ([,var ,val-exp]) ,body)
     ; =>
     (define kaddr* ((alloc time*) exp))
     (define kont (letk var body env kaddr))
     (define store* (store-update store kaddr* (set kont)))
     (values (list (state val-exp env kaddr* time*))
             store*)]
    
    [`(letrec ([,var ,lam]) ,body)
     ; =>
     (define addr ((alloc time*) var))
     (define env* (env-extend env var addr))
     (define clo ((atomic-eval env* store) lam))
     (define store* (store-update store addr clo))
     (values (list (state body env* kaddr time*))
             store*)]
    
    [`(set! ,var ,aexp)
     ; =>     
     (define val ((atomic-eval env store) aexp))
     (define store* (store-update store (env-lookup env var) val))
     (values (list (state '(void) env kaddr time*))
             store*)]
    
    [`(if ,aexp ,true-exp ,false-exp)
     ; =>
     (values
      (for/list ([val (test-eval env store aexp)])
        (if val
            (state true-exp env kaddr time*)
            (state false-exp env kaddr time*)))
      store)]
    
    [`(call/cc ,f)
     (define procs ((atomic-eval env store) f))
     (define params (list (set (kontp kaddr))))
     (for/fold ([states empty] [store store]) ([proc procs])
       (match-define (closure (list (or 'λ 'lambda) formals call) env*) proc)
       (define addrs (map (alloc time*) formals))
       (define env** (env-extend* env* formals addrs))
       (define store* (store-update* store addrs params))
       (values (cons (state call env** kaddr time*) states) store*))]
    
    [(? atomic? aexp)
     ; =>
     (define val ((atomic-eval env store) aexp))
     (for/fold ([states empty] [store store]) ([kont (store-lookup store kaddr)] #:when (not (halt? kont)))
       (match kont
         [(struct letk (var body env* kaddr*))
          (define addr ((alloc time) var))
          (define store* (store-update store addr val))
          (define env** (env-extend env* var addr))
          (values (cons (state body env** kaddr* time*) states) store*)]
         ))]
    
    [`(,f ,args ...)
     ; =>
     (define procs ((atomic-eval env store) f))
     (define params (map (atomic-eval env store) args))
     (for/fold ([states empty] [store store]) ([proc procs])
       (match proc
         [(closure (list (or 'λ 'lambda) formals call) env*)
          (define addrs (map (alloc time*) formals))
          (define env** (env-extend* env* formals addrs))
          (define store* (store-update* store addrs params))
          (values (cons (state call env** kaddr time*) states) store*)]
         [(kontp a)
          (define konts (store-lookup store a))
          (for/fold ([states empty] [store store]) ([kont konts])
            (match kont
              [(struct letk (var body env* kaddr*))
               (define addr ((alloc time) var))
               (define store* (store-update store addr (set)))
               (define env** (env-extend env* var addr))
               (values (cons (state body env** kaddr* time*) states) store*)]))]))]))


;; State-space exploration.

; exlore : set[state] list[state] store -> (values set[state] store regions)
(define (explore seen todo store regions)
  (match todo
    [(list)
     (values seen store regions)]
    [(list-rest (? (curry set-member? seen) st0) todo)
     (explore seen todo store regions)]
    [(list-rest st0 todo)
     (let-values ([(succs new-store) (next st0 store)])
       (define new-regions (assign-regions regions st0 succs store))
       (when output-state-space
         (for ([st1 succs])
           (define mark (mark-of-beast st0))
           (fprintf graph-out "~a -> ~a;~n" mark (mark-of-beast st1))))
       (explore (set-add seen st0) (append succs todo) new-store new-regions))]))

; analyze : exp -> void
(define (analyze exp)
  (define init-state (state exp empty-env (halt) time-zero))
  (define regions (make-regions init-state))
  
  (when output-state-space
    (set! graph-out (open-output-file "state-space.gv" #:exists 'truncate))
    (set! state-out (open-output-file "state-space.txt" #:exists 'truncate)))
  
  (define-values (states store new-regions) (explore empty-set (list init-state) empty-store regions))
  (print-hybrid-automata init-state new-regions store)
  
  (when output-state-space    
    (fprintf graph-out "digraph  {~n")
    (for ([(s r) new-regions])
      (fprintf graph-out "~a [label=\"~a\"];~n" (mark-of-beast s) r))
    (fprintf graph-out "}~n")
    (close-output-port graph-out)
    
    (for ([s states])
      (fprintf state-out "~a :: ~n~a~n" (mark-of-beast s) s))
    (close-output-port state-out)))

(define output-state-space #f)
(define graph-out #f)
(define state-out #f)

(define input #f)

(match (current-command-line-arguments)
  [(vector filename) (set! input (read filename))]
  [else (set! input (read))])

(analyze input)
