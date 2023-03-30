#lang racket

(provide parse
         desugar
         eval
         load-defs
         repl)


;; integer value
(struct int-exp (val) #:transparent)

;; arithmetic expression
(struct arith-exp (op lhs rhs) #:transparent)

;; variable
(struct var-exp (id) #:transparent)

;; let expression
(struct let-exp (ids vals body) #:transparent)

;; lambda expression
(struct lambda-exp (id body) #:transparent)

;; function application
(struct app-exp (fn arg) #:transparent)

;; if statement
(struct if-exp (cond true-exp false-exp) #:transparent)

;; and statement
(struct and-exp (...) #:transparent)

;; or statement
(struct or-exp (...) #:transparent)

;; bool val
(struct bool-exp (bool) #:transparent)

;; relational expression
(struct rel-exp (op lhs rhs) #:transparent)

;; cond expression
(struct cond-exp (bexp rexp) #:transparent)

       


;; Parser
(define (parse sexp)
  (match sexp
    ;; boolean literal
    [(? boolean?)
     (bool-exp sexp)]
    
    ;; integer literal
    [(? integer?)
     (int-exp sexp)]

    ;; arithmetic expression
    [(list (and op (or '+ '* '-)) lhs rhs)
     (arith-exp (symbol->string op) (parse lhs) (parse rhs))]

    ;; if statement
    [(list 'if cond true-exp false-exp)
     (if-exp (parse cond) (parse true-exp) (parse false-exp))]

    ;; and statement
    [(list 'and expr ...)
     (if (null? expr)
         (parse #t)
     (and-exp (map parse expr)))]

    ;; cond expression
    [(list 'cond (list bexp rexp) ...)
     (cond-exp (map parse bexp) (map parse rexp))]

    ;; or statement
    [(list 'or expr ...)
     (if (null? expr)
         (parse #f)
     (or-exp (map parse expr)))]

    ;; relational expressions (<, =, >=, <=, >)
    [(list (and op (or '< '= '>= '<= '>)) lhs rhs)
     (rel-exp (symbol->string op) (parse lhs) (parse rhs))]

    ;; identifier (variable)
    [(? symbol?)
     (var-exp sexp)]

    ;; let expressions
    [(list 'let (list (list id val) ...) body)
     (let-exp (map parse id) (map parse val) (parse body))]

    ;; lambda expression -- modified for > 1 params
    [(list 'lambda (list ids ...) body)
     (lambda-exp ids (parse body))]

    ;; function application -- modified for > 1 args
    [(list f args ...)
     (app-exp (parse f) (map parse args))]

    ;; basic error handling
    [_ (error (format "Can't parse: ~a" sexp))]))



;; Desugar-er -- i.e., syntax transformer
(define (desugar exp)
  (match exp

    ;; relational expressions ">=" "<=" ">"
    ((rel-exp (and op ">=") lhs rhs)
     (desugar (or-exp
               (list (rel-exp "<" rhs lhs) (rel-exp "=" lhs rhs)))))
    
    ((rel-exp (and op "<=") lhs rhs)
     (desugar (or-exp
               (desugar (list (rel-exp "<" lhs rhs) (rel-exp "=" lhs rhs))))))

    ((rel-exp (and op ">") lhs rhs)
     (rel-exp "<" rhs lhs))

    ;; arithmetic expressions "-" and general
    ((arith-exp (and op "-") lhs rhs)
     (arith-exp "+" (desugar lhs) (arith-exp "*" (int-exp -1) (desugar rhs))))
    
    ((arith-exp op lhs rhs)
     (arith-exp op (desugar lhs) (desugar rhs)))

    ;; and-exp
    ((and-exp exprs)
     (foldr (lambda (exp acc) (if-exp (desugar exp) acc (bool-exp #f)))
            (bool-exp #t) exprs))
    
    ;; or-exp
    ((or-exp exprs)
     (foldr (lambda (exp acc) (if-exp (desugar exp) (bool-exp #t) acc))
            (bool-exp #f) exprs))
    ;; if-exp
    ((if-exp cond true-exp false-exp)
     (if-exp (desugar cond) (desugar true-exp) (desugar false-exp)))

    ;; cond-exp
    ((cond-exp bexps rexps)
     (if (not (equal? (var-exp 'else) (last bexps)))
          (error (format "Can't parse, no else : ~a" exp))
      (foldr (lambda (tup acc) (if-exp (first tup) (second tup) acc))
          (desugar (last rexps))
          (map list (desugar (allButLast bexps)) (desugar (allButLast rexps))))))

    ;; let-exp
    ((let-exp ids vals body)
     (let-exp ids (map desugar vals) (desugar body)))

    ;; lambda exp
    ((lambda-exp ids body)
     (foldr (lambda (id lexp) (lambda-exp id lexp))
            (desugar body)
            ids))
    
    ;; function application
    ((app-exp f args)
     (foldl (lambda (id fexp) (app-exp fexp id))
            (desugar f)
            (map desugar args)))
    
    (_ exp)))

;; helper function for cond desugaring
(define (allButLast lst)
  (reverse (rest (reverse lst))))

;; function value + closure
(struct fun-val (id body env) #:prefab)


;; Interpreter
(define (eval expr [env '()])
  (match expr
    
    ;; bool literal
    [(bool-exp bool)
     bool]
    
    ;; int literal
    [(int-exp val) val]

    ;; if statement
    [(if-exp cond expr1 expr2)
     (if (eval cond env) (eval expr1 env) (eval expr2 env))]
    
    ;; relational expression (<)
    [(rel-exp "<" expr1 expr2)
     (< (eval expr1 env) (eval expr2 env))]
    ;; relational expression (=)
    [(rel-exp "=" expr1 expr2)
     (= (eval expr1 env) (eval expr2 env))]

    ;; arithmetic expression
    [(arith-exp "+" lhs rhs)
     (+ (eval lhs env) (eval rhs env))]
 
    ;; multiplication expression   
    [(arith-exp "*" lhs rhs)
     (* (eval lhs env) (eval rhs env))]
    
          
    ;; variable binding
    [(var-exp id)
     (let ([pair (assoc id env)])
       (if pair (cdr pair) (error (format "~a not bound!" id))))]

    ;; let expression
    [(let-exp (list (var-exp id) ...) (list val ...) body)
     (let ([vars (map cons id
                      (map (lambda (v) (eval v env)) val))])
       (eval body (append vars env)))]

    ;; lambda expression
    [(lambda-exp id body)
     (fun-val id body env)]

    ;; function application
    [(app-exp f arg)
     (match-let ([(fun-val id body clenv) (eval f env)]
                 [arg-val (eval arg env)])
       (eval body (cons (cons id arg-val) clenv)))]

    ;; basic error handling
    [_ (error (format "Can't evaluate: ~a" expr))]))



;; load definitions (returning env)
(define (load-defs filename)
  (let* ([sexps (file->list filename)]
         [ph (make-placeholder '())]
         [penv (map (lambda (exp)
                      (cons (first (second exp)) (fun-val (second (second exp)) (desugar (parse (third exp))) ph)))
                    sexps)])
    (placeholder-set! ph penv)
    (make-reader-graph penv)))

;; from lecture notes
(define cyc-env
  (let* ([ph (make-placeholder '())]
         [env (list (cons 'f (fun-val 'x (int-exp 10) ph)))])
    (placeholder-set! ph env)
    (make-reader-graph env)))

;; REPL
(define (repl [filename #f])
  (let loop ([env (if filename (load-defs filename) '())])
    (let ([stx (desugar (parse (read)))])
      (when stx
        (println (eval stx env))
        (loop env)))))