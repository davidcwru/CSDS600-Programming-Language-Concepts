;; Project 2 for CSDS600, Spring 2024
;;
;; Burn it down and start over.
;;
;; Group 04
;; * David Courtney
;; * Joey Houser
;; * Ryan Stack
;;

#lang racket

(require "simpleParser.rkt")

(define uninitialized-vars '(()))

(define M_value
  (lambda (expr state)
    (cond
      ((number? expr) expr)
      ((and (not (pair? expr)) (assigned? expr state)) (variable_value expr state))
      ((and (not (pair? expr)) (declared? expr state)) (error 'error "Using variable before assigning"))
      ((eq? 'true expr) #t)
      ((eq? 'false expr) #f)
      ((not (pair? expr)) (error 'error "Using variable before declaring"))
      ((eq? (op expr) '+) (+ (M_value (a expr) state) (M_value (b expr) state)))
      ((eq? (op expr) '-) (cond
                                       ((null? (operand expr)) (- 0 (M_value (a expr) state)))
                                       (else(- (M_value (a expr) state) (M_value (b expr) state)))))
      ((eq? (op expr) '*) (* (M_value (a expr) state) (M_value (b expr) state)))
      ((eq? (op expr) '/) (quotient (M_value (a expr) state) (M_value (b expr) state)))
      ((eq? (op expr) '%) (remainder (M_value (a expr) state) (M_value (b expr) state)))
      (else (M_boolean expr state)))))

(define M_state
  (lambda (expr state return continue break throw)
    (cond
      ((eq? (function expr) 'return) (return (M_state-return expr state)))
      ((eq? (function expr) 'var) (M_state-declaration expr state))
      ((eq? (function expr) '=) (assign expr state))
      ((eq? (function expr) 'while) (M_state-while expr state return continue break throw))
      ((eq? (function expr) 'if) (M_state-if_else expr state return continue break throw))
      ((eq? (function expr) 'begin) (M_state-block expr state return continue break throw))
      ((eq? (function expr) 'continue) (continue state))
      ((eq? (function expr) 'break) (break (pop_layer state)))
      ((eq? (function expr) 'throw) (throw (add 'exception (M_value (throw_val expr) state) state)))
      ((eq? (function expr) 'try) (M_state-try expr state return continue break throw))
      ((eq? (function expr) 'catch) (M_state-catch expr state return continue break throw))
      ((eq? (function expr) 'finally) (M_state-finally expr state return continue break throw))
      (else (error 'error "Unknown expression type")))))

(define M_boolean
  (lambda (expr state)
    (cond
      ((or (eq? 'true expr) (eq? #t expr)) #t)
      ((or (eq? 'false expr) (eq? #f expr)) #f)
      ((eq? (op expr) '>) (> (M_value (a expr) state) (M_value (b expr) state)))
      ((eq? (op expr) '>=) (>= (M_value (a expr) state) (M_value (b expr) state)))
      ((eq? (op expr) '<) (< (M_value (a expr) state) (M_value (b expr) state)))
      ((eq? (op expr) '<=) (<= (M_value (a expr) state) (M_value (b expr) state)))
      ((eq? (op expr) '!=) (not (eq? (M_value (a expr) state) (M_value (b expr) state))))
      ((eq? (op expr) '==) (eq? (M_value (a expr) state) (M_value (b expr) state)))
      ((eq? (op expr) '||) (or (M_boolean (M_value (a expr) state) state) (M_boolean (M_value (b expr) state) state)))
      ((eq? (op expr) '&&) (and (M_boolean (M_value (a expr) state) state) (M_boolean (M_value (b expr) state) state)))
      ((eq? (op expr) '!) (not (M_boolean (M_value (a expr) state) state))))))


(define M_state-return
  (lambda (expr state)
    (cond
      ((eq? (M_value (return_expr expr) state) #t) 'true)
      ((eq? (M_value (return_expr expr) state) #f) 'false)
      (else (M_value (return_expr expr) state)))))

(define M_state-declaration
  (lambda (expr state)
    (cond
      ((declared? (variable expr) state) (error 'error "redeclaration of variable"))
      ((value? expr) (add (variable expr) (M_value (val expr) state) state))
      (else (add (variable expr) 'null state)))))

(define M_state-while
  (lambda (expr state return continue break throw)
    (call/cc
     (lambda (break)
       (cond
         ((M_boolean (condition expr) state) (M_state expr (call/cc (lambda (continue) (M_state (body expr) state return continue break throw))) return continue break throw))
         ((not (M_boolean (condition expr) state)) state))))))

(define M_state-if_else
  (lambda (expr state return continue break throw)
    (cond
      ((M_boolean (condition expr) state) (M_state (expr1 expr) state return continue break throw))
      ((and (not (M_boolean (condition expr) state)) (null? (else expr))) state)
      ((not (M_boolean (condition expr) state)) (M_state (expr2 expr) state return continue break throw)))))

(define M_state-block
  (lambda (expr state return continue break throw)
    (cond
      ((null? expr) (pop_layer state))
      ((eq? (function expr) 'begin) (M_state-block (rest expr) (push_layer state) return continue break throw))
      (else (M_state-block (rest expr) (M_state (first expr) state return continue break throw) return continue break throw)))))

(define M_state-try
  (lambda (expr state return continue break throw)
    (cond
      ((and (null? (catch_exper expr)) (null? (finally_expr expr))) (error 'error "try without catch and finally"))
      (else (M_state-finally (finally_expr expr) (M_state-catch (catch_exper expr) (call/cc (lambda (throw) (try-helper expr (try_it expr) state return continue break throw))) return continue break throw) return continue break throw)))))

(define M_state-catch
  (lambda (catch_exper state return continue break throw)
       (cond
         ((null? catch_exper) state)
         ((and (eq? (function catch_exper) 'catch) (declared? 'exception state)) (M_state-catch (catch_expr catch_exper) (rename (get_exception catch_exper) state) return continue break throw))
         ((eq? (function catch_exper) 'catch) state)
         (else (M_state-catch (rest catch_exper) (M_state (first catch_exper) state return continue break throw) return continue break throw)))))

(define M_state-finally
  (lambda (expr state return continue break throw)
    (cond
      ((null? expr) state)
      (else (M_state-finally (rest expr) (M_state (first expr) state return continue break throw) return continue break throw)))))


(define try-helper
  (lambda (try expr state return continue break throw)
    (cond
      ((null? expr) state)
      ((eq? 'return (function (first expr))) (M_state (first expr) (M_state-finally (finally_expr try) state return continue break throw) return continue break throw))
      ((eq? 'break (function (first expr))) (M_state (first expr) (M_state-finally (finally_expr try) state return continue break throw) return continue break throw))
      (else (try-helper try (rest expr) (M_state (first expr) state return continue break throw) return continue break throw)))))


(define first car)
(define rest cdr)
(define function car)
(define return_expr cadr)
(define op car)
(define a cadr)
(define b caddr)
(define condition cadr)
(define expr1 caddr)
(define expr2 cadddr)
(define operand cddr)
(define variable cadr)
(define value cddr)
(define first_variable caar)
(define val caddr)
(define variable_def caar)
(define get_value cadar)
(define body caddr)
(define else cdddr)
(define pop_layer cdr)
(define first_layer car)
(define next_layer cdr)
(define name caar)
(define throw_val cadr)
(define catch_expr caddr)
(define try_it cadr)
(define get_exception caadr)

(define push_layer
  (lambda (state)
    (cons '() state)))

(define catch_exper
  (lambda (expr)
    (cond
      ((null? (caddr expr)) '())
      (else (caddr expr)))))

(define finally_expr
  (lambda (cmd)
    (cond
      ((null? (cadddr cmd)) '())
      (else (cadr (cadddr cmd))))))

(define declared?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((declared?-helper var (first state)) #t)
      (else (declared? var (rest state))))))

(define declared?-helper
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((eq? (first_variable state) var) #t)
      (else  (declared?-helper var (rest state))))))

(define value?
  (lambda (expr)
    (cond
      ((null? (value expr)) #f)
      (else #t))))

(define insert
  (lambda (var value state)
    (cond
      ((null? state) '())
      ((declared?-helper var (first state)) (cons (insert-helper var value (first state)) (rest state)))
      (else (cons (first state) (insert var value (rest state)))))))

(define insert-helper
  (lambda (var value state)
    (cond
      ((null? state) '())
      ((eq? var (variable_def state)) (cons (cons var (cons value '())) (insert-helper var value (rest state))))
      (else (cons (first state) (insert-helper var value (rest state)))))))

(define assigned?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((assigned?-helper var (first state)) #t)
      (else (assigned? var (rest state))))))

(define assigned?-helper
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((and (eq? (first_variable state) var) (not (eq? (get_value state) 'null))) #t)
      (else (assigned?-helper var (rest state))))))

(define assign
  (lambda (expr state)
    (cond
      ((declared? (variable expr) state) (insert (variable expr) (M_value (val expr) state) state))
      (else(error 'error "Using variable before declaring")))))

(define add
  (lambda (var value state)
    (cons (append (first state) (cons (cons var (cons value '())) '())) (rest state))))

(define variable_value
  (lambda (var state)
    (cond
      ((null? state) (error 'error "using before declaring"))
      ((number? (variable_value-helper var (first state))) (variable_value-helper var (first state)))
      ((eq? (variable_value-helper var (first state)) #t) #t)
      ((eq? (variable_value-helper var (first state)) #f) #f)
      (else (variable_value var (rest state))))))

(define variable_value-helper
  (lambda (var state)
    (cond
      ((null? state) '())
      ((eq? var (variable_def state)) (get_value state))
      (else (variable_value-helper var (rest state))))))

(define rename
  (lambda (exception state)
    (cons (rename-helper exception (first_layer state)) (next_layer state))))

(define rename-helper
  (lambda (e current)
    (cond
      ((null? current) '())
      ((eq? (name current) 'exception) (cons (cons e (cons (get_value current) '())) (rest current)))
      (else (cons (first current) (rename-helper e (rest current))))))) 

(define process-program
  (lambda (parsed_code state)
    (call/cc
     (lambda (return)
       (cond
         ((null? parsed_code) state)
         (else (process-program (rest parsed_code) (M_state (first parsed_code) state return
                                                            (lambda (v) (error 'error "continue outside of loop"))
                                                            (lambda (v2) (error 'error "break outside of loop"))
                                                            (lambda (v3) (error 'error "throw outside of loop"))
                                                            ))))))))


(define interpret
  (lambda (filename)
    (process-program (parser filename) uninitialized-vars)))

; -----
; Tests
; -----

(displayln "Running tests from Part 1")
(display "Test 01 (Expected output is 150)      Actual Output -> ")
(displayln (interpret "p1_test1.txt"))
(display "Test 02 (Expected output is -4)       Actual Output -> ")
(displayln (interpret "p1_test2.txt"))
(display "Test 03 (Expected output is 10)       Actual Output -> ")
(displayln (interpret "p1_test3.txt"))
(display "Test 04 (Expected output is 16)       Actual Output -> ")
(displayln (interpret "p1_test4.txt"))
(display "Test 05 (Expected output is 220)      Actual Output -> ")
(displayln (interpret "p1_test5.txt"))
(display "Test 06 (Expected output is 5)        Actual Output -> ")
(displayln (interpret "p1_test6.txt"))
(display "Test 07 (Expected output is 6)        Actual Output -> ")
(displayln (interpret "p1_test7.txt"))
(display "Test 08 (Expected output is 10)       Actual Output -> ")
(displayln (interpret "p1_test8.txt"))
(display "Test 09 (Expected output is 5)        Actual Output -> ")
(displayln (interpret "p1_test9.txt"))
(display "Test 10 (Expected output is -39)      Actual Output -> ")
(displayln (interpret "p1_test10.txt"))
(displayln "Test 11 (Expected output is error)    Skipping...  ")
(displayln "Test 12 (Expected output is error)    Skipping...  ")
(displayln "Test 13 (Expected output is error)    Skipping...  ")
(displayln "Test 14 (Expected output is error)    Skipping...  ")
(display "Test 15 (Expected output is true)     Actual Output -> ")
(displayln (interpret "p1_test15.txt"))
(display "Test 16 (Expected output is 100)      Actual Output -> ")
(displayln (interpret "p1_test16.txt"))
(display "Test 17 (Expected output is false)    Actual Output -> ")
(displayln (interpret "p1_test17.txt"))
(display "Test 18 (Expected output is true)     Actual Output -> ")
(displayln (interpret "p1_test18.txt"))
(display "Test 19 (Expected output is 128)      Actual Output -> ")
(displayln (interpret "p1_test19.txt"))
(display "Test 20 (Expected output is 12)       Actual Output -> ")
(displayln (interpret "p1_test20.txt"))
(displayln "End of tests from Part 1")
(displayln "")


(displayln "Running tests from Part 2")
(display "Test 01 (Expected output is 20)       Actual Output -> ")
(displayln (interpret "p2_test1.txt"))
(display "Test 02 (Expected output is 164)      Actual Output -> ")
(displayln (interpret "p2_test2.txt"))
(display "Test 03 (Expected output is 32)       Actual Output -> ")
(displayln (interpret "p2_test3.txt"))
(display "Test 04 (Expected output is 2)        Actual Output -> ")
(displayln (interpret "p2_test4.txt"))
(displayln "Test 05 (Expected output is error)    Skipping...  ")
(display "Test 06 (Expected output is 25)       Actual Output -> ")
(displayln (interpret "p2_test6.txt"))
(display "Test 07 (Expected output is 21)       Actual Output -> ")
(displayln (interpret "p2_test7.txt"))
(display "Test 08 (Expected output is 6)        Actual Output -> ")
(displayln (interpret "p2_test8.txt"))
(display "Test 09 (Expected output is -1)       Actual Output -> ")
(displayln (interpret "p2_test9.txt"))
(display "Test 10 (Expected output is 789)      Actual Output -> ")
(displayln (interpret "p2_test10.txt"))
(displayln "Test 11 (Expected output is error)    Skipping...  ")
(displayln "Test 12 (Expected output is error)    Skipping...  ")
(displayln "Test 13 (Expected output is error)    Skipping...  ")
(display "Test 14 (Expected output is 12)       Actual Output -> ")
(displayln (interpret "p2_test14.txt"))

(display "Test 15 (Expected output is 125)      Actual Output -> ")
(displayln (interpret "p2_test15.txt"))
(display "Test 16 (Expected output is 110)      Actual Output -> ")
(displayln (interpret "p2_test16.txt"))
(display "Test 17 (Expected output is 2000400)  Actual Output -> ")
(displayln (interpret "p2_test17.txt"))
(display "Test 18 (Expected output is 101)      Actual Output -> ")
(displayln (interpret "p2_test18.txt"))
(displayln "Test 19 (Expected output is error)    Skipping...  ")
(displayln "End of tests from Part 2")
(displayln "")


(displayln "Run the following tests manually, and ensure each one generates the appropriate error.")
(displayln "Part 1, Test 11: (displayln (interpret \"p1_test11.txt\"))")
(displayln "Part 1, Test 12: (displayln (interpret \"p1_test12.txt\"))")
(displayln "Part 1, Test 13: (displayln (interpret \"p1_test13.txt\"))")
(displayln "Part 1, Test 14: (displayln (interpret \"p1_test14.txt\"))")
(displayln "Part 2, Test 05: (displayln (interpret \"p2_test5.txt\"))")
(displayln "Part 2, Test 11: (displayln (interpret \"p2_test11.txt\"))")
(displayln "Part 2, Test 12: (displayln (interpret \"p2_test12.txt\"))")
(displayln "Part 2, Test 13: (displayln (interpret \"p2_test13.txt\"))")
(displayln "Part 2, Test 19: (displayln (interpret \"p2_test19.txt\"))")
