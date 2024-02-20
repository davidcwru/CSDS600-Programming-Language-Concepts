#lang racket

(require "simpleParser.rkt")

(define (my-assoc-delete-all key alist)
  (filter (lambda (item) (not (equal? (car item) key))) alist))

(define (lookup var env)
  (let ((pair (assoc var env)))
    (if pair
        (cdr pair)
        (error "Using variable before declaring:" var))))

(define (eval-expr expr env)
  (match expr
    ['true true]
    ['false false]
    [(? number? n) n]
    [(list '- a  ) (- 0 (eval-expr a env))]
    [(list '+ a b) (+ (eval-expr a env) (eval-expr b env))]
    [(list '- a b) (- (eval-expr a env) (eval-expr b env))]
    [(list '* a b) (* (eval-expr a env) (eval-expr b env))]
    [(list '/ a b) (quotient (eval-expr a env) (eval-expr b env))]
    [(list '% a b) (modulo (eval-expr a env) (eval-expr b env))]
    [(? symbol? x) 
     (let ((val (lookup x env)))
       (if (eq? val 'uninitialized)
           (error "Variable used before assigning a value:" x)
           val))]
    [(list '+ a b) 
     (let ((val-a (eval-expr a env))
           (val-b (eval-expr b env)))
       (if (or (eq? val-a 'uninitialized) (eq? val-b 'uninitialized))
           (error "Attempting to use uninitialized variable in addition")
           (+ val-a val-b)))]
    [(list '- a b) 
     (let ((val-a (eval-expr a env))
           (val-b (eval-expr b env)))
       (if (or (eq? val-a 'uninitialized) (eq? val-b 'uninitialized))
           (error "Attempting to use uninitialized variable in subtraction")
           (- val-a val-b)))]
    [(list '* a b) 
     (let ((val-a (eval-expr a env))
           (val-b (eval-expr b env)))
       (if (or (eq? val-a 'uninitialized) (eq? val-b 'uninitialized))
           (error "Attempting to use uninitialized variable in multiplication")
           (* val-a val-b)))]
    [(list '/ a b) 
     (let ((val-a (eval-expr a env))
           (val-b (eval-expr b env)))
       (if (or (eq? val-a 'uninitialized) (eq? val-b 'uninitialized))
           (error "Attempting to use uninitialized variable in division")
           (if (eq? val-b 0)
               (error "Division by zero")
               (quotient val-a val-b))))]
    [(list '% a b) 
     (let ((val-a (eval-expr a env))
           (val-b (eval-expr b env)))
       (if (or (eq? val-a 'uninitialized) (eq? val-b 'uninitialized))
           (error "Attempting to use uninitialized variable in modulo operation")
           (if (eq? val-b 0)
               (error "Modulo by zero")
               (modulo val-a val-b))))]
    [(list '&& a b) (and (eval-expr a env) (eval-expr b env))]
    [(list '|| a b) (or  (eval-expr a env) (eval-expr b env))]
    [(list '<= a b) (<=  (eval-expr a env) (eval-expr b env))]
    [(list '>= a b) (>=  (eval-expr a env) (eval-expr b env))]
    [(list '>  a b) (>   (eval-expr a env) (eval-expr b env))]
    [(list '<  a b) (<   (eval-expr a env) (eval-expr b env))]
    [(list '== a b) (eq? (eval-expr a env) (eval-expr b env))]
    [(list '!= a b) (not (eq? (eval-expr a env) (eval-expr b env)))]
    [(list '!  a)   (not (eval-expr a env))]
    [else (error "Unknown expression type" expr)]))  ; Added expr to error for debugging.


(define (extend var value env)
  (let ((existing (assoc var env)))
    (if existing
        (cons (cons var value) (my-assoc-delete-all var env))
        (cons (cons var value) env))))

(define (exec-stmt stmt env)
  ; Print the statement being executed for debugging, if needed
  ;(displayln stmt)
  (match stmt

    ; Handle variable declaration without initialization
    [(list 'var x) 
     (if (assoc x env)
         (error "Redefining variable:" x)  ; Prevent redeclaration
         (extend x 'uninitialized env))]

    ; Handle variable declaration with initialization
    [(list 'var x expr) 
     (if (assoc x env)
         (error "Redefining variable:" x)  ; Prevent redeclaration
         (extend x (eval-expr expr env) env))]

    ; Handle variable assignment
    [(list '= x expr)
     (if (assoc x env)  ; Ensure variable was declared
         (extend x (eval-expr expr env) env)
         (error "Using variable before declaring:" x))]

    ; Handle return statement
    [(list 'return expr) (extend 'return (eval-expr expr env) env)]

    ; Handle if statement without else
    [(list 'if cond then-stmt) 
     (if (eval-expr cond env)
         (exec-stmt then-stmt env)
         env)]

    ; Handle if statement with else
    [(list 'if cond then-stmt else-stmt) 
     (if (eval-expr cond env)
         (exec-stmt then-stmt env)
         (exec-stmt else-stmt env))]
    [(list 'while cond body)
     (let loop ((env env))
       (if (eval-expr cond env)
           (let ((new-env (exec-stmt body env)))
             (loop new-env))
           env))]  ; Return the environment if the condition is false

    ; Default case for unknown statement types
    [else (error "Unknown statement type" stmt)]))
                                                                                                                                             
;; Main interpreter function
(define interpret
  (lambda (filename)
    (let* ((parsed-program (parser filename))
           (initial-env '()))
      (define (process-program program env)
        (if (null? program)
            (let ((result (lookup 'return env)))
              ;; Convert boolean results to symbols for display
              (cond ((eq? result #t) 'true)  ;; Return 'true as a symbol
                    ((eq? result #f) 'false) ;; Return 'false as a symbol
                    (else result)))  ;; Return non-boolean results as-is
            (let* ((first-stmt (car program))
                   (new-env (exec-stmt first-stmt env)))
              (process-program (cdr program) new-env))))
      (process-program parsed-program initial-env))



  ) ; end lambda
) ; end define
