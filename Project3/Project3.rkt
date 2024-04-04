;; Project 3 for CSDS600, Spring 2024
;;
;; Group 04
;; * David Courtney
;; * Joey Houser
;; * Ryan Stack
;;

#lang racket

(require "functionparser.rkt")

(define eval-expression
  (lambda (expr environment throw)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) "true")
      ((eq? expr 'false) "false")
      ((not (list? expr)) (lookup expr environment))
      (else (evaluateOperator expr environment throw)))))

(define evaluateOperator
  (lambda (expr environment throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment throw)))
      (else (evaluateOperator-helper expr (eval-expression (operand1 expr) environment throw) environment throw)))))

(define evaluateOperator-helper
  (lambda (expr operatorValue environment throw)
    (cond
      ((eq? '+ (operator expr)) (+ operatorValue (eval-expression (operand2 expr) environment throw)))
      ((eq? '- (operator expr)) (- operatorValue (eval-expression (operand2 expr) environment throw)))
      ((eq? '* (operator expr)) (* operatorValue (eval-expression (operand2 expr) environment throw)))
      ((eq? '/ (operator expr)) (quotient operatorValue (eval-expression (operand2 expr) environment throw)))
      ((eq? '% (operator expr)) (remainder operatorValue (eval-expression (operand2 expr) environment throw)))
      ((eq? '== (operator expr)) (isequal? operatorValue (eval-expression (operand2 expr) environment throw)))
      ((eq? '!= (operator expr)) (not (isequal? operatorValue (eval-expression (operand2 expr) environment throw))))
      ((eq? '< (operator expr)) (< operatorValue (eval-expression (operand2 expr) environment throw)))
      ((eq? '> (operator expr)) (> operatorValue (eval-expression (operand2 expr) environment throw)))
      ((eq? '<= (operator expr)) (<= operatorValue (eval-expression (operand2 expr) environment throw)))
      ((eq? '>= (operator expr)) (>= operatorValue (eval-expression (operand2 expr) environment throw)))
      ((eq? '|| (operator expr)) (or operatorValue (eval-expression (operand2 expr) environment throw)))
      ((eq? '&& (operator expr)) (and operatorValue (eval-expression (operand2 expr) environment throw)))
      ((eq? 'funcall (statementType expr)) (evaluateFunctionCall expr environment throw))
      (else (error 'error "Erroneous expression")))))

(define evaluateParameter
  (lambda (parameterValue-list environment throw)
    (if (null? parameterValue-list) '()
        (cons (eval-expression (car parameterValue-list) environment throw) (evaluateParameter (cdr parameterValue-list) environment throw)))))


(define isequal?
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


(define interpretStatements
  (lambda (statements environment return break continue throw)
    (if (null? statements)
        (evaluateFunction environment return break continue throw)
        (interpretStatements (remainingframes statements) (M_state (first statements) environment return break continue throw) return break continue throw))))

(define evaluateFunction
  (lambda (environment return break continue throw)
      (evaluateFunctionCallBody (mainFunctionBody (lookup 'main environment)) (push-frame environment) return break continue throw)))

(define evaluateFunctionCall
  (lambda (statement environment throw)
    (call/cc
     (lambda (func-return)
      (evaluateFunctionCallBody (functionBody (lookup (functionName statement) environment))
      (addParameter (parameterName (lookup (functionName statement) environment)) (evaluateParameter (parameterValue statement) environment throw) (append (push-frame (findScope (functionName statement) environment)) environment))
      func-return
      (lambda (env) (error 'error "'break' statement utilized outside a loop context"))
      (lambda (env) (error 'error "'continue' statement utilized outside a loop context"))
      throw)
      ))))

(define evaluateFunctionCallBody
  (lambda (body environment return break continue throw)
    (cond
      ((null? body) (pop-frame environment))
      ((eq? 'var (statementType (first body))) (evaluateFunctionCallBody (remainingframes body) (evaluateFunctionDeclaration (first body) environment throw) return break continue throw))
      (else (evaluateFunctionCallBody (remainingframes body) (M_state (first body) environment return break continue throw) return break continue throw)))))


(define evaluateFunctionCall-return-environment
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (env-return)
       (evaluateFunctionCallBodyState (functionBody (lookup (functionName statement) environment))
                                         (addParameter (parameterName (lookup (functionName statement) environment)) (evaluateParameter (parameterValue statement) environment throw) (append (push-frame (findScope (functionName statement) environment)) environment))
                                         return
                                         (lambda (env) (error 'error "'break' statement utilized outside a loop context"))
                                         (lambda (env) (error 'error "'continue' statement utilized outside a loop context"))
                                         throw
                                         env-return)))))

(define evaluateFunctionCallBodyState
  (lambda (body environment return break continue throw env-return)
    (cond
      ((null? body) environment)
      ((eq? 'return (currentStatementType body)) (env-return (pop-frame environment)))
      ((eq? 'var (statementType (first  body))) (evaluateFunctionCallBodyState (remainingframes body) (evaluateFunctionDeclaration (first body) environment throw) return break continue throw env-return))
      (else (evaluateFunctionCallBodyState (remainingframes body) (M_state (first body) environment return break continue throw) return break continue throw env-return)))))

(define evaluateFunctionDeclaration
  (lambda (statement environment throw)
    (if (declared?-list (getVar statement) (variables (car environment)))
        (M_state-assign statement environment throw)
        (M_state-declare statement environment throw))))



(define M_state
  (lambda (statement environment return break continue throw)
    (cond
      ((eq? 'return (statementType statement)) (M_state-return statement environment return throw))
      ((eq? 'var (statementType statement)) (M_state-declare statement environment throw))
      ((eq? '= (statementType statement)) (M_state-assign statement environment throw))
      ((eq? 'if (statementType statement)) (M_state-ifElse statement environment return break continue throw))
      ((eq? 'while (statementType statement)) (M_state-while statement environment return throw))
      ((eq? 'continue (statementType statement)) (continue environment))
      ((eq? 'break (statementType statement)) (break environment))
      ((eq? 'begin (statementType statement)) (M_state-block statement environment return break continue throw))
      ((eq? 'throw (statementType statement)) (M_state-throw statement environment throw))
      ((eq? 'try (statementType statement)) (M_state-try statement environment return break continue throw))
      ((eq? 'function (statementType statement)) (M_state-function statement environment return break continue throw))
      ((eq? 'funcall (statementType statement)) (evaluateFunctionCall-return-environment statement environment return break continue throw))
      (else (error 'error "Unrecognized statement" (statementType statement))))))
  
(define M_state-function
  (lambda (statement environment return break continue throw)
    (cond
      ((null? (FunctionsBody statement)) environment)
      (else (insert (cadr statement) (box (FunctionsBodyParameter statement)) environment)))))

(define M_state-return
  (lambda (statement environment return throw)
        (return (eval-expression (get statement) environment throw))))

(define M_state-declare
  (lambda (statement environment throw)
    (if (value? statement)
        (insert (getVar statement) (box (eval-expression (getVal statement) environment throw)) environment)
        (insert (getVar statement) (box 'novalue) environment))))

(define M_state-assign
  (lambda (statement environment throw)
    (update (getLeft statement) (eval-expression (getRight statement) environment throw) environment)))

(define M_state-ifElse
  (lambda (statement environment return break continue throw)
    (cond
      ((eval-expression (get statement) environment throw) (M_state (then statement) environment return break continue throw))
      ((else? statement) (M_state (else statement) environment return break continue throw))
      (else environment))))

(define M_state-while
  (lambda (statement environment return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment throw)
                            (loop condition body (M_state body environment return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get statement) (body statement) environment))))))

(define M_state-block
  (lambda (statement environment return break continue throw)
    (pop-frame (blockBody (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

(define blockBody
  (lambda (body environment return break continue throw)
    (cond
      ((null? body) environment)
      ((null? (cdr body)) (M_state (car body) environment return break continue throw))
      (else (blockBody (cdr body) (M_state (car body) environment return break continue throw) return break continue throw)))))

(define M_state-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get statement) environment throw) environment)))

(define throwCatch
  (lambda (catchStatement environment return break continue throw jump finallyBlock)
    (cond
      ((null? catchStatement) (lambda (ex env) (throw ex (M_state-block finallyBlock env return break continue throw)))) 
      ((not (eq? 'catch (statementType catchStatement))) (error 'error "Improper 'catch' statement"))
      (else (lambda (ex env)
              (jump (M_state-block finallyBlock
                                     (pop-frame (throwCatchBody 
                                                 (body catchStatement) 
                                                 (insert (catchVariable catchStatement) (box ex) (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

(define throwCatchBody
  (lambda (body environment return break continue throw)
    (cond
      ((null? body) environment)
      (else (throwCatchBody (remainingframes body) (M_state (first body) environment return break continue throw) return break continue throw)))))

(define M_state-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finallyBlock (finallyBlock (finally statement)))
              (try-block (tryBlock (try statement)))
              (new-return (lambda (v) (begin (M_state-block finallyBlock environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (M_state-block finallyBlock env return break continue throw))))
              (new-continue (lambda (env) (continue (M_state-block finallyBlock env return break continue throw))))
              (new-throw (throwCatch (catch statement) environment return break continue throw jump finallyBlock)))
         (M_state-block finallyBlock
                          (M_state-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

(define tryBlock
  (lambda (statement)
    (cons 'begin statement)))

(define finallyBlock
  (lambda (statement)
    (cond
      ((null? statement) '(begin))
      ((not (eq? (statementType statement) 'finally)) (error 'error "Improperly structured 'finally' block"))
      (else (cons 'begin (cadr statement))))))


(define findScope
  (lambda (function environment)
    (cond
      ((null? environment) (error 'findScope "Scope not identified"))
      ((declared?-list function (caar environment)) environment)
      (else (findScope function (cdr environment))))))

(define push-frame
  (lambda (environment)
    (cons (newFrame) environment)))

(define pop-frame
  (lambda (environment)
    (cdr environment)))

(define topframe car)
(define remainingframes cdr)

(define declared?-environment
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((declared?-list var (variables (topframe environment))) #t)
      (else (declared?-environment var (remainingframes environment))))))

(define declared?-list
  (lambda (var list)
    (cond
      ((null? list) #f)
      ((eq? var (car list)) #t)
      (else (declared?-list var (cdr list))))))


(define lookup
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (error 'error "variable used without an assigned value")
          value))))

(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (error 'error "Variable not defined"))
      ((declared?-list var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (declared?-list var (variables frame))) (error 'error "Variable not defined"))
      (else (getValue (indexOf var (variables frame)) (store frame))))))

(define indexOf
  (lambda (var list)
    (cond
      ((null? list) 0)
      ((eq? var (car list)) 0)
      (else (+ 1 (indexOf var (cdr list)))))))

(define getValue
  (lambda (n l)
    (cond
      ((zero? n) (unbox (car l)))
      (else (getValue (- n 1) (cdr l))))))

(define insert
  (lambda (var val environment)
    (if (declared?-list var (variables (car environment)))
        (error 'error "Reassigning an already defined variable")
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

(define update
  (lambda (var val environment)
    (if (declared?-environment var environment)
        (update-existing var val environment)
        (error 'error "Variable referenced but not declared"))))

(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons val (store frame)))))

(define update-existing
  (lambda (var val environment)
    (if (declared?-list var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

(define update-in-frame-store
  (lambda (var val variableList valueListt)
    (cond
      ((eq? var (car variableList)) (updateBox var val variableList valueListt))
      (else (cons (car valueListt) (update-in-frame-store var val (cdr variableList) (cdr valueListt)))))))

(define updateBox
  (lambda (var val variableList valueListt)
    (begin
      (set-box! (car valueListt) val) valueListt)))
      
(define addParameter
  (lambda (parameter parameterValueue environment)
    (cond
      ((and (null? parameter) (null? parameterValueue)) environment)
      ((or (null? parameter) (null? parameterValueue)) (error 'error "Incorrect number of arguments provided to function."))
      (else (addParameter (cdr parameter) (cdr parameterValueue) (insert (car parameter) (box (car parameterValueue)) environment))))))

(define newstate
  (lambda ()
    (list (newFrame))))

(define newFrame
  (lambda ()
    '(() ())))
	
;-----------------
; HELPER FUNCTIONS
;-----------------

(define first car)

(define statementType car)
(define variables car)
(define store cadr)

(define mainFunctionBody cadr)
(define operator car)
(define operand1 cadr)
(define operand2 caddr)


(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

(define get cadr)
(define getVar cadr)
(define getVal caddr)
(define value? exists-operand2?)
(define getLeft cadr)
(define getRight caddr)

(define then caddr)
(define else cadddr)
(define body caddr)
(define else? exists-operand3?)
(define try cadr)
(define catch caddr)
(define finally cadddr)


(define functionBody cadr)
(define currentStatementType caar)
(define parameterName car)
(define parameterValue cddr)
(define functionName cadr)
(define FunctionsBodyParameter cddr)
(define FunctionsBody cdddr)

(define catchVariable
  (lambda (catchStatement)
    (car (operand1 catchStatement))))

(define interpret
  (lambda (filename)
    (call/cc
      (lambda (return)
        (interpretStatements (parser filename) (newstate) return
                                  (lambda (env) (error 'error "'break' statement utilized outside a loop context"))
                                  (lambda (env) (error 'error "'continue' statement utilized outside a loop context"))
                                  (lambda (v env) (error 'error "'throw' statement utilized outside a loop context")))))))



; -----
; Tests
; -----
(displayln "Part 3 Tests")
(display "Test 01 (Expected output is 10)       Actual Output -> ")
(displayln (interpret "p3_test1.txt"))
(display "Test 02 (Expected output is 14)       Actual Output -> ")
(displayln (interpret "p3_test2.txt"))
(display "Test 03 (Expected output is 45)       Actual Output -> ")
(displayln (interpret "p3_test3.txt"))
(display "Test 04 (Expected output is 55)       Actual Output -> ")
(displayln (interpret "p3_test4.txt"))
(display "Test 05 (Expected output is 1)        Actual Output -> ")
(displayln (interpret "p3_test5.txt"))
(display "Test 06 (Expected output is 115)      Actual Output -> ")
(displayln (interpret "p3_test6.txt"))
(display "Test 07 (Expected output is true)     Actual Output -> ")
(displayln (interpret "p3_test7.txt"))
(display "Test 08 (Expected output is 20)       Actual Output -> ")
(displayln (interpret "p3_test8.txt"))
(display "Test 09 (Expected output is 24)       Actual Output -> ")
(displayln (interpret "p3_test9.txt"))
(display "Test 10 (Expected output is 2)        Actual Output -> ")
(displayln (interpret "p3_test10.txt"))
(display "Test 11 (Expected output is 35)       Actual Output -> ")
(displayln (interpret "p3_test11.txt"))
(displayln "Test 12 (Expected output is error)    Skipping...      ")
(display "Test 13 (Expected output is 90)       Actual Output -> ")
(displayln (interpret "p3_test13.txt"))
(display "Test 14 (Expected output is 69)       Actual Output -> ")
(displayln (interpret "p3_test14.txt"))
(display "Test 15 (Expected output is 87)       Actual Output -> ")
(displayln (interpret "p3_test15.txt"))
(display "Test 16 (Expected output is 64)       Actual Output -> ")
(displayln (interpret "p3_test16.txt"))
(displayln "Test 17 (Expected output is error)    Skipping...      ")
(display "Test 18 (Expected output is 125)      Actual Output -> ")
(displayln (interpret "p3_test18.txt"))
(display "Test 19 (Expected output is 100)      Actual Output -> ")
(displayln (interpret "p3_test19.txt"))
(display "Test 20 (Expected output is 2000400)  Actual Output -> ")
(displayln (interpret "p3_test20.txt"))
(displayln "End of tests from Part 3")
(displayln "")


(displayln "Run the following tests manually, and ensure each one generates the appropriate error.")
(displayln "Test 12: (displayln (interpret \"p3_test12.txt\"))")
(displayln "Test 17: (displayln (interpret \"p3_test17.txt\"))")
