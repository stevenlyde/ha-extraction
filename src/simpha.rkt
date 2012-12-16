#lang racket

;;;

(define (step1 ha)  
  (match ha
    [`(,vars (automaton ,name (initially ,i) ,synclabs ,locs ...))
     `(,vars (automaton ,name (initially ,i) ,synclabs ,@(reduce-locations locs i)))]))

(define (reduce-locations locs init)
  (define-values (names name-to-loc) (map-locs locs))
  (define new-names (append (remove init names) (list init)))
  (hash-values ($reduce-locations new-names name-to-loc)))

(define ($reduce-locations names name-to-loc)
  (if (empty? names)
      name-to-loc
      ($reduce-locations (cdr names) (merge-if-possible (car names) name-to-loc))))


;;;

(define (step2 ha)
  (match ha
    [`(,vars (automaton ,name (initially ,i) ,synclabs ,locs ...))
     `(,vars (automaton ,name (initially ,i) ,synclabs ,@(copy-if-needed locs)))]))

(define (copy-if-needed locs)
  (define-values (names name-to-loc) (map-locs locs))
  (define tmp ($copy-if-needed name-to-loc))
  (hash-values tmp))

(define ($copy-if-needed name-to-loc)
  (for/fold ([name-to-loc name-to-loc]) ([(name loc) name-to-loc])
    (define child (should-copy? name-to-loc loc))
    (if child
        (copy-loc name-to-loc name child)
        name-to-loc)))


;;;

(define (step3 ha)
  (match ha
    [`(,vars (automaton ,name (initially ,i) ,synclabs ,locs ...))
     `(,vars (automaton ,name (initially ,i) ,synclabs ,@(compact locs)))]))

(define (compact locs)
  (define-values (names name-to-loc) (map-locs locs))
  (hash-values ($compact name-to-loc)))

(define ($compact name-to-loc)
  (for/fold ([name-to-loc name-to-loc]) ([(name loc) name-to-loc])
    (define child (single-unguarded-edge? loc))
    (if (and child (hash-has-key? name-to-loc name) (hash-has-key? name-to-loc child))
        (merge name-to-loc (hash-ref name-to-loc name) (hash-ref name-to-loc child))
        name-to-loc)))


;;;

(define (propogate-flows ha)
  (match ha
    [`(,vars (automaton ,name (initially ,i) ,synclabs ,locs ...))
     `(,vars (automaton ,name (initially ,i) ,synclabs ,@($propogate-flows locs i)))]))

(define ($propogate-flows locs init)
  (define-values (names name-to-loc) (map-locs locs))
  (hash-values
   (for/fold ([name-to-loc name-to-loc]) ([(name loc) name-to-loc])
     (p-flows name-to-loc (list name) empty))))

(define (p-flows name-to-loc todo seen)
  (match todo
    [(? empty?) name-to-loc]
    [(list-rest (? (curryr member seen)) rest)
     (p-flows name-to-loc rest seen)]
    [(list-rest name rest)
     (define loc (hash-ref name-to-loc name))
     (define parent-flows (flows-of loc))
     (define new-name-to-loc
       (for/fold ([name-to-loc name-to-loc])
         ([child (children-of loc name-to-loc)])
         (hash-set name-to-loc (cadr child) (update-flow child (merge-flows parent-flows (flows-of child))))))
     (p-flows new-name-to-loc (append todo (child-names loc)) (cons name seen))]))

(define (merge-flows parent child)
  (match parent
    [(? empty?) child]
    [(list-rest f rest)
     (merge-flows rest 
                  (if (contains-flow? f child)
                      child
                      (cons f child)))]))

(define (contains-flow? f lst)
  (match-define `(= ,var ,val) f)
  (not (empty? (filter
                (λ (f2)
                  (match f2
                    [`(= ,var2 ,val2) (equal? var var2)]
                    [else #f]))
                lst))))


;;; Utility functions

(define (map-locs locs)
  (for/fold ([names empty]
             [name-to-loc (hash)])
    ([loc locs])
    (define name (cadr loc))
    (values (cons name names) (hash-set name-to-loc (cadr loc) loc))))


(define (merge-if-possible name name-to-loc)
  (if (hash-has-key? name-to-loc name) 
      (let* ([loc (hash-ref name-to-loc name)]
             [children (children-of loc name-to-loc)])
        (for/fold ([name-to-loc name-to-loc])
          ([child children] #:when (and (mergable? loc child) (unguarded? loc (cadr child))))
          (merge name-to-loc loc child)))
      name-to-loc))

(define (children-of loc name-to-loc)
  (for/list ([name (child-names loc)])
    (hash-ref name-to-loc name)))

(define (child-names loc)
  (match loc
    [`(loc ,name ,inv ,flow (when ,guard ... (goto ,child)) ...) child]))

(define (mergable? loc1 loc2)
  (define inv1 (caddr loc1))
  (define inv2 (caddr loc2))
  (define flow1 (cadddr loc1))
  (define flow2 (cadddr loc2))
  (and (equal? inv1 inv2) (equal? flow1 flow2)))

(define (unguarded? loc child)
  (match loc
    [`(loc ,name ,inv ,flow (when ,guard1 (goto ,child1)) ... (when True (goto ,(? (curry equal? child)))) (when ,guard2 (goto ,child2)) ...) #t]
    [else #f]))


(define (should-copy? name-to-loc loc)
  (define child (single-unguarded-edge? loc))
  (and child (no-flow? (hash-ref name-to-loc child)) child))

(define (single-unguarded-edge? loc)
  (match loc
    [`(loc ,name ,inv ,flow (when True (goto ,child))) child]
    [else #f]))

(define (no-flow? loc)
  (match loc
    [`(loc ,name ,inv (wait) ,whens ...) #t]
    [else #f]))


(define (copy-loc name-to-loc parent-name child-name)
  (define new-name (gensym 's))
  (define parent (hash-ref name-to-loc parent-name))
  (define child (hash-ref name-to-loc child-name))
  (let* ([name-to-loc (hash-set name-to-loc new-name (rename-loc child new-name))]
         [name-to-loc (hash-set name-to-loc parent-name (update-links parent child-name new-name))])
    name-to-loc))

(define (rename-loc loc name)
  (match loc
    [`(loc ,old-name ,inv ,flow ,whens ...)
     `(loc ,name ,inv ,flow ,@whens)]))

(define (update-links loc old-name new-name)
  (define (update-link w)
    (match w
      [`(when ,pred-update-sync ... (goto ,name))
       `(when ,@pred-update-sync (goto ,(if (eq? old-name name) new-name name)))]))
  (match loc
    [`(loc ,name ,inv ,flow ,children ...)
     `(loc ,name ,inv ,flow ,@(map update-link children))]))


(define (merge name-to-loc loc1 loc2)
  (define name1 (cadr loc1))
  (define name2 (cadr loc2))
  (define pre-loc (update-flow loc1 (flows-of loc2)))
  (define new-loc (update-whens pre-loc name2 (whens-of loc2)))
  (define new-hash
    (for/fold ([h (hash)])
      ([(k v) (hash-set name-to-loc name1 new-loc)])
      (define updated-loc (update-links v name2 name1))
      (hash-set h k updated-loc)))
  (hash-remove new-hash name2))

(define (update-flow loc flows)
  (match loc
    [`(loc ,name ,inv (wait ,flow ...) ,children ...)
     `(loc ,name ,inv (wait ,@(set->list (apply set (append flow flows)))) ,@children)]))

(define (flows-of loc)
  (match loc
    [`(loc ,name ,inv (wait ,flow ...) ,children ...) flow]))

(define (update-whens loc merged-name whens)
  (match loc
    [`(loc ,name ,inv ,flow ,children ...)
     (define new-children (filter (λ (w) ((compose not eq?) (target-of w) merged-name)) children))
     (define new-whens (set->list (set-union (apply set new-children) (apply set whens))))
     `(loc ,name ,inv ,flow ,@new-whens)]))

(define (target-of w)
  (match w
    [`(when ,pred-update-sync ... (goto ,name)) name]))

(define (whens-of loc) 
  (match loc
    [`(loc ,name ,inv ,flow ,children ...) children]))


(define (print-hytech ha)
  (match ha
    [`((var ,vars ...) (automaton ,name (initially ,i) (synclabs) ,locs ...))     
     (printf "var ")
     (for ([var vars])
       (printf "~a: ~a; " (car var) (cadr var)))
     (printf "~n")
     (printf "automaton ~a~n" name)
     (printf "synclabs:;~n")
     (printf "initially ~a;~n" i)
     (for ([loc locs])
       (print-loc loc))
     (printf "end~n")]))

(define (print-loc loc)
  (match loc
    [`(loc ,name ,inv ,flow (when ,preds ... (goto ,child-names)) ...)
     (printf "loc ~a: while ~a wait {~a}~n" name (pred-of inv) (format-flows flow))
     (for ([pred preds]
           [child-name child-names])
       (match pred
         [(list pred) (printf "  when ~a goto ~a;~n" (predicate pred) child-name)]
         [(list pred update) (printf "  when ~a ~a goto ~a;~n" (predicate pred) (format-update update) child-name)]))]))

(define (format-flows f)
  (define flows (cdr f))
  (if (empty? flows)
      ""
      (for/fold ([s (format "~a" (predicate (car flows)))])
        ([i (cdr flows)])
        (format "~a,~a" s (predicate i)))))

(define (pred-of p)
  (if (empty? (cdr p))
      ""
      (predicate (cadr p))))

(define-match-expander relop
  (syntax-rules ()
    [(_ p) (and p (or '< '<= '= '>= '> '== '!=))]))

(define (predicate p)
  (match p
    [(? symbol?) p]
    [(? number?) p]
    [`(,(relop r) ,e1 ,e2) (format "~a~a~a" (predicate e1) r (predicate e2))]))

(define (format-update update)
  (match update
    [`(do (= ,var ,val)) (format "do {~a' = ~a}" var val)]))



(define simplify (compose propogate-flows step3 step2 step1))

(define input #f)

(match (current-command-line-arguments)
  [(vector filename) (set! input (read filename))]
  [else (set! input (read))])

(define ha (simplify input))

(print-hytech ha)
