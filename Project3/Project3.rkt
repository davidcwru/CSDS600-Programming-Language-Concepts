;; Project 3 for CSDS600, Spring 2024
;;
;; Group 04
;; * David Courtney
;; * Joey Houser
;; * Ryan Stack
;;

#lang racket

(require "functionparser.rkt")

(define evaluateExpression
  (lambda (expr state throw)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) "true")
      ((eq? expr 'false) "false")
      ((not (list? expr)) (lookup expr state))
      (else (evaluateOperator expr state throw)))))
(define evaluateOperator
  (lambda (expr state throw)
    (cond
      ((eq? '! (operator expr)) (not (evaluateExpression (operand1 expr) state throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (evaluateExpression (operand1 expr) state throw)))
      (else (evaluateOperator-helper expr (evaluateExpression (operand1 expr) state throw) state throw)))))

(define evaluateOperator-helper
  (lambda (expr operatorValue state throw)
    (cond
      ((eq? '+ (operator expr)) (+ operatorValue (evaluateExpression (operand2 expr) state throw)))
      ((eq? '- (operator expr)) (- operatorValue (evaluateExpression (operand2 expr) state throw)))
      ((eq? '* (operator expr)) (* operatorValue (evaluateExpression (operand2 expr) state throw)))
      ((eq? '/ (operator expr)) (quotient operatorValue (evaluateExpression (operand2 expr) state throw)))
      ((eq? '% (operator expr)) (remainder operatorValue (evaluateExpression (operand2 expr) state throw)))
      ((eq? '== (operator expr)) (isEqual? operatorValue (evaluateExpression (operand2 expr) state throw)))
      ((eq? '!= (operator expr)) (not (isEqual? operatorValue (evaluateExpression (operand2 expr) state throw))))
      ((eq? '< (operator expr)) (< operatorValue (evaluateExpression (operand2 expr) state throw)))
      ((eq? '> (operator expr)) (> operatorValue (evaluateExpression (operand2 expr) state throw)))
      ((eq? '<= (operator expr)) (<= operatorValue (evaluateExpression (operand2 expr) state throw)))
      ((eq? '>= (operator expr)) (>= operatorValue (evaluateExpression (operand2 expr) state throw)))
      ((eq? '|| (operator expr)) (or operatorValue (evaluateExpression (operand2 expr) state throw)))
      ((eq? '&& (operator expr)) (and operatorValue (evaluateExpression (operand2 expr) state throw)))
      ((eq? 'funcall (statementType expr)) (evaluateFunctionCall expr state throw))
      (else (error 'error "Erroneous expression")))))

(define evaluateParameter
  (lambda (parameterValue-list state throw)
    (if (null? parameterValue-list) '()
        (cons (evaluateExpression (car parameterValue-list) state throw) (evaluateParameter (cdr parameterValue-list) state throw)))))


(define isEqual?
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


(define interpretStatements
  (lambda (statements state return break continue throw)
    (if (null? statements)
        (evaluateFunction state return break continue throw)
        (interpretStatements (rest statements) (M_state (first statements) state return break continue throw) return break continue throw))))

(define evaluateFunction
  (lambda (state return break continue throw)
      (evaluateFunctionCallBody (mainFunctionBody (lookup 'main state)) (pushFrame state) return break continue throw)))

(define evaluateFunctionCall
  (lambda (statement state throw)
    (call/cc
     (lambda (func-return)
      (evaluateFunctionCallBody (functionBody (lookup (functionName statement) state))
      (addParameter (parameterName (lookup (functionName statement) state)) (evaluateParameter (parameterValue statement) state throw) (append (pushFrame (findScope (functionName statement) state)) state))
      func-return
      (lambda (env) (error 'error "'break' statement utilized outside a loop context"))
      (lambda (env) (error 'error "'continue' statement utilized outside a loop context"))
      throw)
      ))))

(define evaluateFunctionCallBody
  (lambda (body state return break continue throw)
    (cond
      ((null? body) (popFrame state))
      ((eq? 'var (statementType (first body))) (evaluateFunctionCallBody (rest body) (evaluateFunctionDeclaration (first body) state throw) return break continue throw))
      (else (evaluateFunctionCallBody (rest body) (M_state (first body) state return break continue throw) return break continue throw)))))


(define evaluateFunctionCall-return-state
  (lambda (statement state return break continue throw)
    (call/cc
     (lambda (env-return)
       (evaluateFunctionCallBodyState (functionBody (lookup (functionName statement) state))
                                         (addParameter (parameterName (lookup (functionName statement) state)) (evaluateParameter (parameterValue statement) state throw) (append (pushFrame (findScope (functionName statement) state)) state))
                                         return
                                         (lambda (env) (error 'error "'break' statement utilized outside a loop context"))
                                         (lambda (env) (error 'error "'continue' statement utilized outside a loop context"))
                                         throw
                                         env-return)))))

(define evaluateFunctionCallBodyState
  (lambda (body state return break continue throw env-return)
    (cond
      ((null? body) state)
      ((eq? 'return (currentStatementType body)) (env-return (popFrame state)))
      ((eq? 'var (statementType (first  body))) (evaluateFunctionCallBodyState (rest body) (evaluateFunctionDeclaration (first body) state throw) return break continue throw env-return))
      (else (evaluateFunctionCallBodyState (rest body) (M_state (first body) state return break continue throw) return break continue throw env-return)))))

(define evaluateFunctionDeclaration
  (lambda (statement state throw)
    (if (declared?-list (getVar statement) (variables (car state)))
        (M_state-assign statement state throw)
        (M_state-declare statement state throw))))



(define M_state
  (lambda (statement state return break continue throw)
    (cond
      ((eq? 'return (statementType statement)) (M_state-return statement state return throw))
      ((eq? 'var (statementType statement)) (M_state-declare statement state throw))
      ((eq? '= (statementType statement)) (M_state-assign statement state throw))
      ((eq? 'if (statementType statement)) (M_state-ifElse statement state return break continue throw))
      ((eq? 'while (statementType statement)) (M_state-while statement state return throw))
      ((eq? 'continue (statementType statement)) (continue state))
      ((eq? 'break (statementType statement)) (break state))
      ((eq? 'begin (statementType statement)) (M_state-block statement state return break continue throw))
      ((eq? 'throw (statementType statement)) (M_state-throw statement state throw))
      ((eq? 'try (statementType statement)) (M_state-try statement state return break continue throw))
      ((eq? 'function (statementType statement)) (M_state-function statement state return break continue throw))
      ((eq? 'funcall (statementType statement)) (evaluateFunctionCall-return-state statement state return break continue throw))
      (else (error 'error "Unrecognized statement" (statementType statement))))))
  
(define M_state-function
  (lambda (statement state return break continue throw)
    (cond
      ((null? (FunctionsBody statement)) state)
      (else (insert (cadr statement) (box (FunctionsBodyParameter statement)) state)))))

(define M_state-return
  (lambda (statement state return throw)
        (return (evaluateExpression (get statement) state throw))))

(define M_state-declare
  (lambda (statement state throw)
    (if (value? statement)
        (insert (getVar statement) (box (evaluateExpression (getVal statement) state throw)) state)
        (insert (getVar statement) (box 'novalue) state))))

(define M_state-assign
  (lambda (statement state throw)
    (update (getLeft statement) (evaluateExpression (getRight statement) state throw) state)))

(define M_state-ifElse
  (lambda (statement state return break continue throw)
    (cond
      ((evaluateExpression (get statement) state throw) (M_state (then statement) state return break continue throw))
      ((else? statement) (M_state (else statement) state return break continue throw))
      (else state))))

(define M_state-while
  (lambda (statement state return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body state)
                        (if (evaluateExpression condition state throw)
                            (loop condition body (M_state body state return break (lambda (env) (break (loop condition body env))) throw))
                         state))))
         (loop (get statement) (body statement) state))))))

(define M_state-block
  (lambda (statement state return break continue throw)
    (popFrame (blockBody (cdr statement)
                                         (pushFrame state)
                                         return
                                         (lambda (env) (break (popFrame env)))
                                         (lambda (env) (continue (popFrame env)))
                                         (lambda (v env) (throw v (popFrame env)))))))

(define blockBody
  (lambda (body state return break continue throw)
    (cond
      ((null? body) state)
      ((null? (cdr body)) (M_state (car body) state return break continue throw))
      (else (blockBody (cdr body) (M_state (car body) state return break continue throw) return break continue throw)))))

(define M_state-throw
  (lambda (statement state throw)
    (throw (evaluateExpression (get statement) state throw) state)))

(define throwCatch
  (lambda (catchStatement state return break continue throw jump finallyBlock)
    (cond
      ((null? catchStatement) (lambda (ex env) (throw ex (M_state-block finallyBlock env return break continue throw)))) 
      ((not (eq? 'catch (statementType catchStatement))) (error 'error "Improper 'catch' statement"))
      (else (lambda (ex env)
              (jump (M_state-block finallyBlock
                                     (popFrame (throwCatchBody 
                                                 (body catchStatement) 
                                                 (insert (catchVariable catchStatement) (box ex) (pushFrame env))
                                                 return 
                                                 (lambda (env2) (break (popFrame env2))) 
                                                 (lambda (env2) (continue (popFrame env2))) 
                                                 (lambda (v env2) (throw v (popFrame env2)))))
                                     return break continue throw)))))))

(define throwCatchBody
  (lambda (body state return break continue throw)
    (cond
      ((null? body) state)
      (else (throwCatchBody (rest body) (M_state (first body) state return break continue throw) return break continue throw)))))

(define M_state-try
  (lambda (statement state return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finallyBlock (finallyBlock (finally statement)))
              (try-block (tryBlock (try statement)))
              (new-return (lambda (v) (begin (M_state-block finallyBlock state return break continue throw) (return v))))
              (new-break (lambda (env) (break (M_state-block finallyBlock env return break continue throw))))
              (new-continue (lambda (env) (continue (M_state-block finallyBlock env return break continue throw))))
              (new-throw (throwCatch (catch statement) state return break continue throw jump finallyBlock)))
         (M_state-block finallyBlock
                          (M_state-block try-block state new-return new-break new-continue new-throw)
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
  (lambda (function state)
    (cond
      ((null? state) (error 'findScope "Scope not identified"))
      ((declared?-list function (caar state)) state)
      (else (findScope function (cdr state))))))

(define pushFrame
  (lambda (state)
    (cons (newFrame) state)))

(define popFrame
  (lambda (state)
    (cdr state)))

(define topFrame car)
(define rest cdr)

(define declared?-state
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((declared?-list var (variables (topFrame state))) #t)
      (else (declared?-state var (rest state))))))

(define declared?-list
  (lambda (var list)
    (cond
      ((null? list) #f)
      ((eq? var (car list)) #t)
      (else (declared?-list var (cdr list))))))


(define lookup
  (lambda (var state)
    (let ((value (lookupState var state)))
      (if (eq? 'novalue value)
          (error 'error "variable used without an assigned value")
          value))))
(define lookupState
  (lambda (var state)
    (cond
      ((null? state) (error 'error "Variable not defined"))
      ((declared?-list var (variables (topFrame state))) (lookupFrame var (topFrame state)))
      (else (lookupState var (cdr state))))))

(define lookupFrame
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
  (lambda (var val state)
    (if (declared?-list var (variables (car state)))
        (error 'error "Reassigning an already defined variable")
        (cons (addtoFrame var val (car state)) (cdr state)))))

(define update
  (lambda (var val state)
    (if (declared?-state var state)
        (updateBinding var val state)
        (error 'error "Variable referenced but not declared"))))

(define addtoFrame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons val (store frame)))))

(define updateBinding
  (lambda (var val state)
    (if (declared?-list var (variables (car state)))
        (cons (updateFrame var val (topFrame state)) (rest state))
        (cons (topFrame state) (updateBinding var val (rest state))))))

(define updateFrame
  (lambda (var val frame)
    (list (variables frame) (updateFrameStore var val (variables frame) (store frame)))))

(define updateFrameStore
  (lambda (var val variableList valueListt)
    (cond
      ((eq? var (car variableList)) (updateBox var val variableList valueListt))
      (else (cons (car valueListt) (updateFrameStore var val (cdr variableList) (cdr valueListt)))))))

(define updateBox
  (lambda (var val variableList valueListt)
    (begin
      (set-box! (car valueListt) val) valueListt)))
      
(define addParameter
  (lambda (parameter parameterValueue state)
    (cond
      ((and (null? parameter) (null? parameterValueue)) state)
      ((or (null? parameter) (null? parameterValueue)) (error 'error "Incorrect number of arguments provided to function."))
      (else (addParameter (cdr parameter) (cdr parameterValueue) (insert (car parameter) (box (car parameterValueue)) state))))))

(define newstate
  (lambda ()
    (list (newFrame))))

(define newFrame
  (lambda ()
    '(() ())))

(define first car)


(define statementType car)
(define variables car)
(define store cadr)

(define mainFunctionBody cadr)
(define operator car)
(define operand1 cadr)
(define operand2 caddr)


(define checkforOperand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define checkforOperand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

(define get cadr)
(define getVar cadr)
(define getVal caddr)
(define value? checkforOperand2?)
(define getLeft cadr)
(define getRight caddr)

(define then caddr)
(define else cadddr)
(define body caddr)
(define else? checkforOperand3?)
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
