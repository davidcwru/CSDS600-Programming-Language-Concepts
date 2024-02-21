;; Project 2 for CSDS600, Spring 2024
;;
;; Group 04
;; * David Courtney
;; * Joey Houser
;; * Ryan Stack
;;

#lang racket

(require "simpleParser.rkt")

(define my-assoc-delete-all
  (lambda (key alist)
    (filter (lambda (item) (not (equal? (car item) key))) alist)
  ) ; end lambda
) ; end define

(define lookup
  (lambda (var env)
    (let ((pair (assoc var env)))
      (if pair
        (cdr pair)
        (error "Using variable before declaring:" var))
    ) ; end let
  ) ; end lambda
) ; end define

(define eval-expr
  (lambda (expr env)
    (define binary-op
      (lambda (op a b)
        (let ((val-a (eval-expr a env))
          (val-b (eval-expr b env)))
          (if (or (eq? val-a 'uninitialized) (eq? val-b 'uninitialized))
            (error "Attempting to use uninitialized variable in operation")
          (if (and (eq? op '/) (eq? val-b 0))
            (error "Division by zero")
            (if (and (eq? op '%) (eq? val-b 0))
                (error "Modulo by zero")
                (case op
                  ('+ (+ val-a val-b))
                  ('- (- val-a val-b))
                  ('* (* val-a val-b))
                  ('/ (quotient val-a val-b))
                  ('% (modulo val-a val-b))
                ) ; end case
              ) ; end if
            ) ; end if 
          ) ; end if
        ) ; end let
      ) ; end lambda
    ) ; end define

    (define lookup-variable
      (lambda (x)
        (let ((val (lookup x env)))
          (if (eq? val 'uninitialized)
            (error "Variable used before assigning a value:" x)
            val
           ) ; end if
        ) ; end let
      ) ; end lambda
    ) ; end define

    (match expr
      ['true true]
      ['false false]
      [(? number? n) n]
      [(? symbol? x) (lookup-variable x)]
      [(list '-  a)   (- 0 (eval-expr a env))]
      [(list '+  a b) (binary-op '+ a b)]
      [(list '-  a b) (binary-op '- a b)]
      [(list '*  a b) (binary-op '* a b)]
      [(list '/  a b) (binary-op '/ a b)]
      [(list '%  a b) (binary-op '% a b)]
      [(list '&& a b) (and (eval-expr a env) (eval-expr b env))]
      [(list '|| a b) (or  (eval-expr a env) (eval-expr b env))]
      [(list '<= a b) (<=  (eval-expr a env) (eval-expr b env))]
      [(list '>= a b) (>=  (eval-expr a env) (eval-expr b env))]
      [(list '>  a b) (>   (eval-expr a env) (eval-expr b env))]
      [(list '<  a b) (<   (eval-expr a env) (eval-expr b env))]
      [(list '== a b) (eq? (eval-expr a env) (eval-expr b env))]
      [(list '!= a b) (not (eq? (eval-expr a env) (eval-expr b env)))]
      [(list '!  a)   (not (eval-expr a env))]
      [else (error "Unknown expression type" expr)]

    ) ; end match
  ) ; end lambda
) ; end define

(define extend
  (lambda (var value env)
    (let ((existing (assoc var env)))
      (if existing
          (cons (cons var value) (my-assoc-delete-all var env))
          (cons (cons var value) env)
      )
    ) ; end let
  ) ; end lambda
) ; end define

(define exec-stmt
  (lambda (stmt env)
  ; (displayln stmt) ; Print the statement being executed for debugging
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

    [(list 'return expr) 
       (let ((ret-value (eval-expr expr env)))
         (values env ret-value 'return))]  ; Return a flag indicating execution of a return

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

      ; Handle 'begin' statement
      [(list 'begin stmts ...)
       (define (exec-sequence statements environment)
         (if (null? statements)
             environment  ; No more statements, return the environment
             (let ((new-env (exec-stmt (car statements) environment)))
               (exec-sequence (cdr statements) new-env))))
       (exec-sequence stmts env)]


    ; Default case for unknown statement types
    [else (error "Unknown statement type" stmt)])
  ) ; end lambda
) ; end define

;; Main interpreter function
(define interpret
  (lambda (filename)
    (let* ((parsed-program (parser filename))  ; Start parsing the input file
           (initial-env '()))  ; Initialize an empty environment
      (define process-program
        (lambda (program env)
          (if (null? program)  ; Check if there are no more statements
              (let ((result (lookup 'return env)))  ; Look for a return value in the environment
                (cond ((eq? result #t) 'true)  ; Return 'true' for true boolean
                      ((eq? result #f) 'false) ; Return 'false' for false boolean
                      (else result)))  ; Return any other non-boolean value as is
              (let* ((first-stmt (car program))  ; Extract the first statement from the program
                     (exec-result (exec-stmt first-stmt env))  ; Execute the statement
                     (new-env (first exec-result))  ; Update the environment
                     (return-flag (third exec-result)))  ; Check if a return statement was encountered
                (if (eqv? return-flag 'return)  ; If a return was encountered,
                    (second exec-result)  ; then return its value
                    (process-program (cdr program) new-env))))))  ; Otherwise, continue with the rest of the program
      (process-program parsed-program initial-env))))  ; Begin processing the program


