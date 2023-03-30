#lang racket

(provide (all-defined-out)) ; export all top-level definitions for testing

(require racket/trace)


;; 1. Integer exponentiation
(define (iexpt n e)
  (if (= e 0)
      1
    (if (= e 1)
        n
   (* n (iexpt n (sub1 e))))))

;; 2. Polynomial evaluation

(define (poly-eval coeffs x)
   (define reversedList (reverse coeffs))
   (letrec ([polynomial (lambda (coeffs x exp)
           (if (empty? coeffs)
               0
               (+ (* (first coeffs) (iexpt x (sub1 exp))) (polynomial (rest coeffs) x (sub1 exp)))))])
     (polynomial reversedList x (length coeffs))))

;; 3. List concatenation

(define (concatenate . lsts)
  (concatenateHelper lsts '() '()) )

(define (concatenateHelper lsts currentList accumulator)
  (cond
    [(and (empty? currentList) (empty? lsts)) (reverse accumulator)]
    [(empty? currentList) (concatenateHelper (rest lsts) (first lsts) accumulator)]
    [else (concatenateHelper lsts (rest currentList) (cons (first currentList) accumulator))]))
        


;; 4. List ordered merging (non-tail-recursive)
(define (merge l1 l2)
   (cond
        [(and (empty? l1) (empty? l2)) empty]
        [(empty? l1) (cons (first l2) (merge l1 (rest l2)))]  ;
        [(empty? l2) (cons (first l1) (merge (rest l1) l2))]
        [else
          (let* ([element1 (first l1)]
                 [element2 (first l2)])
            (cond [(< element1 element2) (cons element1 (merge (rest l1) l2))] 
                  [else  (cons element2 (merge l1 (rest l2)))]))]))


;; 5. List ordered merging (tail-recursive)
(define (merge-tail l1 l2)
  (define newList '())
  (mergeHelper l1 l2 newList))

(define (mergeHelper l1 l2 newList)
  (cond
         [(and (empty? l1) (empty? l2)) (reverse newList)]
         [(empty? l1) (mergeHelper l1 (rest l2) (cons (first l2) newList))]  ;
         [(empty? l2) (mergeHelper (rest l1) l2 (cons (first l1) newList))]
         [else
          (let* ([element1 (first l1)]
                 [element2 (first l2)])
            (cond [(< element1 element2) (mergeHelper (rest l1) l2 (cons element1 newList))] 
                  [else (mergeHelper l1 (rest l2) (cons element2 newList))]))]))


;; 6. List run-length encoding
(define (run-length-encode lst)
  (define couple '())
  (if (empty? lst)
      lst
  (encode (rest (reverse lst)) '() (cons (first (reverse lst)) couple) 1)))

(define (encode currentlst lst couple n)
     (cond 
           [(empty? currentlst) (cons (reverse (cons n couple)) lst)] ;if the currentLst is empty, append the couple reversed
           [(equal? (first couple) (first currentlst)) (encode (rest currentlst) lst couple (+ n 1))]
           [else (encode (rest currentlst) (cons (reverse (cons n couple)) lst) (list (first currentlst)) 1)]))
        
        




;; 7. List run-length decoding
(define (run-length-decode lst)
  (if (empty? lst)
      lst
  (decode lst '())))

(define (decode lst accumulator)
   (if (empty? lst)
       accumulator
          (let* ([updatedlst (concatenate accumulator (newlst (first lst) '() (second (first lst))))])
            (decode (rest lst) updatedlst))))

(define (newlst couple lst n)
  (if (= n 0)
      lst
        (newlst couple (cons (first couple) lst) (- n 1))))      
        
                 

;; 8. Labeling sexps
(define (label-sexp sexp)
    (match sexp
      [(? integer?)
       `(int ,sexp)]

      [(? symbol?)
       `(var ,sexp)]

      [(list (and op (or '+ '* '/ '-)) lhs rhs)
       `(arith (op ,op) ,(label-sexp lhs) ,(label-sexp rhs))]
      
      [(list name arg)
       `(funcall (name ,name) ,(label-sexp arg))]

      [_ (error "Can't label!")]))
      