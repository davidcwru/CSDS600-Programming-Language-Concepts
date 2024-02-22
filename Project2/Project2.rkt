;; Project 2 for CSDS600, Spring 2024
;;
;; Group 04
;; * David Courtney
;; * Joey Houser
;; * Ryan Stack
;;

#lang racket

(require "simpleParser.rkt")

(define uninitialized-vars '())

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
  (lambda (var env)
    (if (member var uninitialized-vars)
        (error "Variable used before being initialized: " var)
        (let ((val (lookup var env)))
          (if (eq? val 'uninitialized)
              (error "Variable used before being initialized: " var)
              val)))))



    (match expr
      ['true true]
      ['false false]
      [(? number? n) n]
      [(? symbol? x) (lookup-variable x env)]
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
          (cons (cons var (if (equal? value 'uninitialized) 'uninitialized value)) env)))))


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
             (if (assoc 'break new-env)
                 (my-assoc-delete-all 'break new-env)  ; Remove 'break' and exit loop
                 (if (assoc 'return new-env)
                     new-env  ; Return new environment if 'return' is encountered
                     (loop new-env))))  ; Continue with next iteration
           env))]  ; Exit loop if condition is false


    ; Set a flag for break in the environment
    [(list 'break)
     (extend 'break true env)]


    ; Set a flag for continue in the environment
    [(list 'continue) 
     (extend 'continue true env)]  

    ; Handle 'begin' statement
      [(list 'begin stmts ...)
(define (exec-sequence statements environment)
  (if (null? statements)
      environment  ; No more statements, return the environment
      (let ((new-env (exec-stmt (car statements) environment)))
        (if (or (assoc 'return new-env) (assoc 'continue new-env) (assoc 'break new-env))
            new-env  ; Stop executing further and return the environment
            (exec-sequence (cdr statements) new-env)))))


       (exec-sequence stmts env)]


    ; Default case for unknown statement types
    [else (error "Unknown statement type" stmt)])
  ) ; end lambda
) ; end define

;; Main interpreter function
(define interpret
  (lambda (filename)
    (let* ((parsed-program (parser filename))  ; Parse the program from the file
           (initial-env '()))  ; Start with an empty environment
      (define process-program
        (lambda (program env)
          (if (null? program)
              (let ((result (lookup 'return env)))  ; Look for a 'return' in the environment
                (cond ((eq? result #t) 'true)  ; Convert boolean results to symbols
                      ((eq? result #f) 'false)
                      (else result)))  ; Return non-boolean results as-is
              (let* ((first-stmt (car program))  ; Process the first statement
                     (new-env (exec-stmt first-stmt env)))
                (if (assoc 'return new-env)  ; Check if there is a 'return' in the new environment
                    (let ((return-value (lookup 'return new-env)))  ; Extract the return value
                      (cond ((eq? return-value #t) 'true)  ; Convert boolean results to symbols, if necessary
                            ((eq? return-value #f) 'false)
                            (else return-value)))  ; Return the extracted value
                    (process-program (cdr program) new-env)))))  ; Otherwise, continue processing
      ) ; end define
      (process-program parsed-program initial-env)  ; Start the program processing
    ) ; end let
  ) ; end lambda
) ; end define




; -----
; Tests
; -----

(displayln "Run tests from Part 1")
(display "Test 01 (Expected output is 150)    Actual Output -> ")
(displayln (interpret "p1_test1.txt"))
(display "Test 02 (Expected output is -4)     Actual Output -> ")
(displayln (interpret "p1_test2.txt"))
(display "Test 03 (Expected output is 10)     Actual Output -> ")
(displayln (interpret "p1_test3.txt"))
(display "Test 04 (Expected output is 16)     Actual Output -> ")
(displayln (interpret "p1_test4.txt"))
(display "Test 05 (Expected output is 220)    Actual Output -> ")
(displayln (interpret "p1_test5.txt"))
(display "Test 06 (Expected output is 5)      Actual Output -> ")
(displayln (interpret "p1_test6.txt"))
(display "Test 07 (Expected output is 6)      Actual Output -> ")
(displayln (interpret "p1_test7.txt"))
(display "Test 08 (Expected output is 10)     Actual Output -> ")
(displayln (interpret "p1_test8.txt"))
(display "Test 09 (Expected output is 5)      Actual Output -> ")
(displayln (interpret "p1_test9.txt"))
(display "Test 10 (Expected output is -39)    Actual Output -> ")
(displayln (interpret "p1_test10.txt"))
(display "Test 15 (Expected output is true)   Actual Output -> ")
(displayln (interpret "p1_test15.txt"))
(display "Test 16 (Expected output is 100)    Actual Output -> ")
(displayln (interpret "p1_test16.txt"))
(display "Test 17 (Expected output is false)  Actual Output -> ")
(displayln (interpret "p1_test17.txt"))
(display "Test 18 (Expected output is true)   Actual Output -> ")
(displayln (interpret "p1_test18.txt"))
(display "Test 19 (Expected output is 128)    Actual Output -> ")
(displayln (interpret "p1_test19.txt"))
(display "Test 20 (Expected output is 12)     Actual Output -> ")
(displayln (interpret "p1_test20.txt"))
(displayln "End of tests from Part 1")
(displayln "")


(displayln "Run all tests from Part 2")
(display "Test 01 (Expected output is 20)     Actual Output -> ")
(displayln (interpret "test1.txt"))
(display "Test 02 (Expected output is 164)    Actual Output -> ")
(displayln (interpret "test2.txt"))
(display "Test 03 (Expected output is 32)     Actual Output -> ")
(displayln (interpret "test3.txt"))
(display "Test 04 (Expected output is 2)      Actual Output -> ")
(displayln (interpret "test4.txt"))
(display "Test 05 (Expected output is error)  Actual Output -> ")
(displayln (interpret "test5.txt"))
(display "Test 06 (Expected output is 25)     Actual Output -> ")
(displayln (interpret "test6.txt"))
(display "Test 07 (Expected output is 21)     Actual Output -> ")
(displayln (interpret "test7.txt"))
(display "Test 08 (Expected output is 6)      Actual Output -> ")
(displayln (interpret "test8.txt"))
(display "Test 09 (Expected output is -1)     Actual Output -> ")
(displayln (interpret "test9.txt"))
(displayln "End of tests")


;(interpret "test9.txt")

