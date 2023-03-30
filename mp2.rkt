#lang racket

(provide (all-defined-out)) ; export all top-level definitions


;;;;; Part 1: HOFs (and some utility functions)

;doritos
(define (deep-map fn lst)
  (cond
        [(empty? lst) '()]
        [(list? lst) (print lst) (cons (deep-map fn (first lst)) (deep-map fn (rest lst)))]
        [(pair? lst) (cons (deep-map fn (car lst)) (deep-map fn (cdr lst)))]
        [else (fn lst) ]))


;burritos
(define (my-curry fn . rest)
  (cond [(= (procedure-arity fn) 0) fn]
        [(= (procedure-arity fn) (length rest)) (apply fn rest)]
        [else  (my-curry-helper fn (reverse rest))]))

(define (my-curry-helper fn args)
  (lambda id
    (cond
      [(list? id)
       (cond [(= (procedure-arity fn) (+ (length args) (length id))) (apply fn (reverse (append id args)))]
             [else (my-curry-helper fn (append id args))])]
      [else
       (cond [(= (procedure-arity fn) (length (cons id args))) (apply fn (reverse (cons id args)))]
             [(my-curry-helper fn (cons id args))])])))


;hi
(define (lookup key lst)
  (cond
    [(empty? lst) #f];if we didn't find the key, we return false
    [else (let*
              ([tup (car lst)]
               [potentialKey (car tup)])
            (if (equal? potentialKey key)
                (cdr tup)
                (lookup key (cdr lst))))]))
    


(define (update key value lst)
  ;handling case of no key and no value
  (if (and (null? key) (null? value))
      #f
      (update-helper key value lst lst)))


;boo
(define (update-helper key value lst org)
  (cond
    [(empty? lst) (append org (list (cons key value)))]
    [else (let*
              ([tup (car lst)]
               [potentialKey (car tup)]
               )
            (if (equal? potentialKey key)
                (begin
                  (set! lst (remove tup lst))
                  (cons (cons key value) lst))
                (update-helper key value (cdr lst) org)))]))

;handles function case
(define (handle-obj-fn key fn lst)
  (cond
    [(empty? lst) "error"]
    [(equal? (car (first lst)) key) (cons (cons key (fn (cdr (first lst)))) (rest lst))]
    [else (cons (first lst) (handle-obj-fn key fn (rest lst)))]
   )
 )


(define (make-object name)
  (let ([attributes (cons(cons 'name name) '())])

    ;getting the attribute specified
    (define (get arg)
      (lookup arg attributes))

    ;handles the setting cases
    (define (set arg1 arg2)
      (set! attributes
            (update arg1 arg2 attributes)))
    ;updates the current object
    (define (update-obj arg1 arg2)
      (set! attributes
            (handle-obj-fn arg1 arg2 attributes)))
    ;pattern match against all cases
    (define (pattern-match . args)
      (let ([f (first args)])
        (cond [(equal? f 'get) (get (second args))]
              [(equal? f 'set) (set (second args) (third args))]
              [(equal? f 'update) (update-obj (second args) (third args))]
              [else void])))
    pattern-match)
  )
;;;;; Part 2: Symbolic differentiation (no automated tests!)

(define (diff var exp)
  (cond [(not (list? exp)) (if (eq? exp var)
                               1
                               0)]

        [(eq? (first exp) '+)
                  (cons '+ (map (curry diff var) (rest exp)))] 
        [(eq? (first exp) '*) `
                  (+ (* ,(second exp) ,(diff var (third exp)) ) (* ,(third exp) ,(diff var (second exp)) ) )]

        [(eq? (first exp) '^) (if (eq? (second exp) var)
                                  `(* ,(third exp) (^ ,(second exp) ,(- (third exp) 1)))
                                  0)]))

;;;;; Part 3: Meta-circular evaluator


(define (my-eval rexp)
  (let my-eval-env ([rexp rexp]
                    [env '()])       
    (cond [(symbol? rexp)                
           (lookup rexp env)]
          [(eq? (first rexp) 'lambda)  
           (lambda (id) (my-eval-env (third rexp) (update (first (second rexp)) id env)))]
                                         

          [else                         
           ((my-eval-env (first rexp) env) (my-eval-env (second rexp) env))])))


;;;;; Part 4: Free variables

(define (free sexp)
  (let my-free-env ([sexp sexp]
                    [env '()])           ; environment (assoc list)
    (cond [(and (symbol? sexp)  (not (lookup sexp env)))          ; variable
           (list sexp)]
          [(eq? (first sexp) 'lambda)    ; lambda expression
           (let
                ([f (third sexp)]
                 [x (first (second sexp))])
               (remove x (my-free-env f env))    
             )]
          [else                          ; function application
               (append (my-free-env (first sexp) env)
                       (my-free-env (second sexp) env))
             ])))


;;;;; Extra credit: algebraic simplification (add your own tests)

;; Implemented features:

;checks to see if there is a value in the lst, a specified value, in this case its #f
(define (value-in-list? val list)
	(cond [(null? list) #f]
		[(equal? (car list) val) #t]
		[else (value-in-list? val (cdr list))]))

;this functtion checks to see if the expression contains only numbers and operational symbols
;it does so by going through all the values and removing them one by one
;stacking them in an accumulator
;since I don't know how to return immediattly in racket, i had it go through the accumulator to find a false value
(define (check-all-num? exp acc)
  (cond
    [(empty? exp) (if (value-in-list? #f (reverse acc))
                      #f
                      #t)]
    [(integer? (first exp)) (check-all-num? (cdr exp) (cons (first exp) acc))]
    [(symbol? (first exp))  (cond
                              [(eq? '+ (car exp)) (check-all-num? (cdr exp) (cons (car exp) acc))]
                              [(eq? '* (car exp)) (check-all-num? (cdr exp) (cons (car exp) acc))]
                              [else #f])]
    [else (check-all-num? (cdr exp) (cons (check-all-num? (car exp) '()) acc))]))


(define (simplify exp)
  ;if the current expression, is all numbers, then we perform eval on the current exp
      (if (check-all-num? exp '())
          (eval exp)
          ;calls helper to handle case of not all number
          (helper-fn exp)))


;could have added the cases to cover the symbols but this is too much! Tried mapping the function to cdr but ran into issues
  (define (helper-fn exp)
      (cond
        ;handles expressions of size three
        [(or (not (list? exp)) (not (= 3 (length exp)))) exp]
        ;handling of addittion cases for number and zero below (simplifying all cases below)
        [(and (eq? '+ (first exp)) (and (number? (helper-fn (second exp))) (zero? (helper-fn (second exp))))) (helper-fn (third exp))]
        [(and (eq? '+ (first exp)) (and (number? (helper-fn (third exp))) (zero? (helper-fn (third exp))))) (helper-fn (second exp))]
        ;handling both multiplication cases for number and 1 multiplication(simplified)
        [(and (eq? '* (first exp)) (and (number? (helper-fn (second exp))) (= 1 (helper-fn (second exp)))))  (helper-fn (third exp))]
        [(and (eq? '* (first exp)) (and (number? (helper-fn (third exp))) (= 1 (helper-fn (third exp))))) (helper-fn (second exp))]
        ;handling both multiplication cases for number and zero(simplified)
        [(and (eq? '* (first exp)) (and (number? (helper-fn (second exp))) (zero? (helper-fn (second exp))))) 0]
        [(and (eq? '* (first exp)) (and (number? (helper-fn (third exp))) (zero? (helper-fn (third exp))))) 0]
        ;below we handle all cases where the second and third expression is a number
        [(and (eq? '+ (first exp)) (number? (helper-fn (second exp))) (number? (helper-fn (third exp)))) (+ (helper-fn (second exp)) (helper-fn (third exp)))]
        [(and (eq? '+ (first exp)) (number? (helper-fn (second exp))) (number? (helper-fn (third exp)))) (+ (helper-fn (second exp)) (helper-fn (third exp)))]
        [(and (eq? '* (first exp)) (number? (helper-fn (second exp))) (number? (helper-fn (third exp)))) (* (helper-fn (second exp)) (helper-fn (third exp)))]
        [(and (eq? '* (first exp)) (number? (helper-fn (second exp))) (number? (helper-fn (third exp)))) (* (helper-fn (second exp)) (helper-fn (third exp)))]
        [else  exp]
    
            ))