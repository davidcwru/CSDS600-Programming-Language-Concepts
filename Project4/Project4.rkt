;; Project 4 for CSDS600, Spring 2024
;;
;; Group 04
;; * David Courtney
;; * Joey Houser
;; * Ryan Stack
;;

#lang racket

(require "classParser.rkt")
(require racket/trace)

(define parse-tree
  (lambda (inputfile)
    (parser inputfile)))

(define M_state
  (lambda (exp environment s return break continue try catch finally)
    (cond
      [(null? exp) s]

      [(not (list? (firstExpression exp))) (M_define_type exp environment s return break continue try catch finally)]
      [(null? (expressionRemainder exp))          (M_define_type (firstExpression exp) environment s
                                                        return break continue try catch finally)]

      [(eq? (firstExpression exp) 'begin)  (pop-frame (lambda (k) (M_state (expressionRemainder exp)
                                                                       (push-frame environment) s return k continue
                                                                       try catch finally)))]
      [else (M_state (expressionRemainder exp)
                                                    (M_define_type (firstExpression exp) environment s return break
                                                                 continue try catch finally) s
                                                    return break continue try catch finally)])))

(define pop-frame
  (lambda (environment)
    (list (environment-class-name environment) (environment-super environment) (nextlayer (environment-body environment)))))

(define push-frame
  (lambda (environment)
    (list (environment-class-name environment) (environment-super environment) (list (cons new-layer (local (environment-body environment))) (global (environment-body environment))))))

(define M_state_function
  (lambda (exp environment s return break continue try catch finally)
    (cond
      [(null? exp)s]
      [(null? (expressionRemainder exp))           (M_state_function (firstExpression exp) environment s
                                                         return break continue try catch finally)]

      [(eq? (statement-type-id exp) 'class) (m-add-class exp s)]

      [(eq? (statement-type-id exp) 'function)
                                            (m-add-global-func (full-func exp) (list (append (list (func-name exp))
                                                                                    (list (func-body exp)))) environment)]

      [(eq? (statement-type-id exp) 'var)   (m-var-dec exp environment s)]

      [else                                 (M_state_function (expressionRemainder exp) environment
                                                         (M_state_function (firstExpression exp) environment s return break
                                                                       continue try catch finally)
                                                         return break continue try catch finally)])))


(define M_define_type
  (lambda (exp environment s return break continue try catch finally)
    (cond
      [(or (null? exp) (not (pair? exp)))      s]

      [(eq? (statement-type-id exp) 'function) (m-add-local-func (full-func exp)
                                                                  (list (append (list (func-name exp))
                                                                              (list (func-body exp))))
                                                                  environment
                                                          s)]

      [(and (eq? (statement-type-id exp) 'funcall)
       (and (list? (funcall-name exp))
            (null? (func-params exp))))
                   (m-update (dot-var-name exp) (m-dot-func (dot-var-name exp)  (dot-var-name exp) (dot-func-name exp) no-params environment s (lambda (v) s)) environment s)]

      [(and (eq? (statement-type-id exp) 'funcall)
            (list? (funcall-name exp)))
                   (m-update (dot-var-name exp) (m-dot-func (dot-var-name exp) (dot-func-name exp) (func-params exp) environment s (lambda (v) s)) environment s)]

      [(and (eq? (statement-type-id exp) 'funcall) (null? (func-params exp)))
                                               (m-funcall (funcall-name exp) no-params (lambda (v) environment) s)]

      [(eq? (statement-type-id exp) 'funcall)
                                               (m-funcall (funcall-name exp) (func-params exp) (lambda (v) environment) environment s)]

      [(eq? (firstExpression exp) 'begin)      (pop-frame (M_state (expressionRemainder exp) (push-frame environment) s
                                                               return break continue try catch finally))]

      [(eq? (statement-type-id exp) 'if)       (m-if-statement exp environment s return break continue try catch finally)]
      [(eq? (statement-type-id exp) 'while)    (call/cc (lambda (k) (m-while-loop exp environment s return k continue
                                                                                  try catch finally)))]


      [(eq? (statement-type-id exp) 'break)    (break (pop-frame environment))]
      [(eq? (statement-type-id exp) 'continue) (continue environment)]

      [(eq? (statement-type-id exp) 'try)      (call/cc (λ (k) (m-try-catch-finally exp environment s return break
                                                                                    continue k catch
                                                                                    finally)))]

      [(eq? (statement-type-id exp) 'throw)    (try (pop-frame (catch (statement-body exp))))]

      [(eq? (statement-type-id exp) 'var)      (m-var-dec exp environment s)]

      [(eq? (statement-type-id exp) '=)        (m-assign exp environment s)]

      [(eq? (statement-type-id exp) 'return)   (m-return (statement-body exp) environment s return finally)]

      [else                                    (error 'undefined "undefined expression")])))


(define funcall-function-name caddr)
(define funcall-instance-name cadr)


(define m-funcall
  (lambda (name actual return environment s)
    (cond
      [(and (list? name) (eq? (cadr name) 'this))  (m-update (funcall-function-name name) (m-funcall (funcall-function-name name) actual return environment s) environment s)]
      [(list? name)                               (m-update (funcall-instance-name name)
                                                            (m-dot-func (funcall-instance-name name) (funcall-function-name name) actual  environment s return)
                                                            environment s)]
      [else
        (let* [(all (m-lookup-func name environment s))
               (formal (func-formal-params all))
               (body (func-call-body all))]
          (if (eq? (num-in-list actual 0) (num-in-list formal 0))
              (M_state body (lists-to-assign actual formal (push-frame environment) s) s
                       return
                       (lambda (v) v)
                       (lambda (v) v)
                       (lambda (v) v)
                       (lambda (v) v)
                       (lambda (v) v))
              (error 'undefined "Paramater mismatch")))])))


(define lists-to-assign
  (lambda (l1 l2 environment s)
    (cond
      [(null? l1)            environment]
      [(and (not (number? (car l1))) (> (num-in-list l1 0) 1))
                    (lists-to-assign (list-from-state l1 environment s) l2 environment s)]
      

      [else (lists-to-assign (cdr l1) (cdr l2)
                                     (m-var-dec (cons 'var (cons (car l2) (list (car l1)))) environment s) s)])))

(define list-from-state
  (lambda (lis environment s)
    (cond
      [(null? lis) '()]
      [(not (number? (car lis))) (cons (m-lookup-var (car lis) environment s) (list-from-state (cdr lis) environment s))]
      [else (cons (car lis) (list-from-state (cdr lis) environment s))])))


(define num-in-list
  (lambda (lis acc)
    (if (null? lis)
        acc
        (num-in-list (cdr lis) (+ acc 1)))))

(define m-try-catch-finally
  (lambda (exp environment s return break continue try catch finally)
    (cond
      [(and (not (pair? (third-statement exp))) (not (pair? (catch-statement exp))))
       (error 'undefined "try statement missing catch or finally")]


      [(and (not (pair? (third-statement exp))) (eq? (second-identifier exp) 'catch))
       (call/cc (lambda (k) (M_state (try-body exp) environment s return break continue k

                                     (lambda (exception) (M_state (catch-body (second-body exp))

                                                                  (m-var-dec (list 'var (catch-var-name
                                                                                         (second-body exp))
                                                                                   exception) (push-frame environment) s)
                                                                  s
                                                                  return break continue k catch finally))
                                     finally)))]

      [(and (eq? (third-identifier exp) 'finally) (not (pair? (catch-statement exp))))
       (M_state (third-body exp) (M_state (try-body exp) environment s return break continue
                                          (lambda (v) environment) (lambda (v) environment) finally)
                s
                return break continue try catch finally)]


      [(and (eq? (second-identifier exp) 'catch) (eq? (third-identifier exp) 'finally))
       (M_state (third-body exp)
                (call/cc (lambda (k) (M_state (try-body exp) environment s return break continue k

                                              (lambda (exception) (M_state (catch-body (second-body exp))
                                                                           (m-var-dec
                                                                            (list 'var
                                                                                  (catch-var-name
                                                                                   (second-body exp))
                                                                                  exception) (push-frame environment) s)
                                                                           s
                                                                           return k continue
                                                                           try catch finally)) finally)))
                s
                return break continue try catch finally)]
      [else
       (error 'undefined "try statement missing catch or finally")])))


(define M_value
  (lambda (exp environment s)
    (cond
      [(null? exp)                            (error 'undefined "undefined expression")]
      [(number? exp)                          exp]

      [(and (and (list? exp) (eq? (car exp) 'dot)) (eq? (cadr exp) 'this))
                                              (lookup-global-var (caddr exp) (environment-body environment) environment s)]
      [(and (not (pair? exp)) (boolean? exp)) exp]

      [(eq? exp 'true) #t]
      [(eq? exp 'false) #f]


      [(and (pair? exp) (am-i-boolean exp))   (M_condition exp environment s)]


      [(and (pair? exp) (and (eq? (statement-type-id exp) 'funcall) (null? (func-params exp))))
                                              (call/cc (lambda (k) (m-funcall (funcall-name exp) '() k environment s)))]


      [(and (pair? exp) (eq? (statement-type-id exp) 'funcall))
                                              (call/cc (lambda (k) (m-funcall (funcall-name exp) (func-params exp) k environment s)))]



      [(not (pair? exp))                      (m-lookup-var exp environment s)]


      [(and (pair? exp) (eq? (statement-type-id exp) 'dot)) (m-dot-value (operand1 exp) (dot-variable-name exp) environment s)]



      [(and (pair? exp) (and (eq? (statement-type-id exp) 'funcall) (null? (func-params exp))))
                                              (M_value (m-funcall (funcall-name exp) '() (λ(v) v) s) s)]


      [(and (pair? exp) (eq? (statement-type-id exp) 'funcall))
                                              (M_value (m-funcall (funcall-name exp) (func-params exp) (λ(v) v) s) s)]




      [(eq? (operator exp) '+) (+         (M_value (left-operand exp) environment s) (M_value (operand2 exp) environment s))]
      [(and (eq? (operator exp) '-) (null? (operand2-exists exp)))
                               (* -1      (M_value (left-operand exp) environment s))]
      [(eq? (operator exp) '-) (-         (M_value (left-operand exp) environment s) (M_value (operand2 exp) environment s))]
      [(eq? (operator exp) '*) (*         (M_value (left-operand exp) environment s) (M_value (operand2 exp) environment s))]
      [(eq? (operator exp) '/) (quotient  (M_value (left-operand exp) environment s) (M_value (operand2 exp) environment s))]
      [(eq? (operator exp) '%) (remainder (M_value (left-operand exp) environment s) (M_value (operand2 exp) environment s))]


      [else                    (error 'undefined "undefined expression")])))


(define M_condition
  (lambda (exp environment s)
    (cond

      [(null? exp)               (error 'undefined "undefined expression")]
      [(not (pair? exp))         (M_value exp environment s)]
      [(null? (operator exp))    (M_value exp environment s)]

      

      [(eq? (operator exp) '||)  (or  (M_condition (left-operand exp) environment s) (M_condition (operand2 exp) environment s))]
      [(eq? (operator exp) '&&)  (and (M_condition (left-operand exp) environment s) (M_condition (operand2 exp) environment s))]
      [(eq? (operator exp) '!)   (not (M_condition (left-operand exp) environment s))]

      [(eq? (operator exp) '==)  (eq? (M_condition (left-operand exp) environment s) (M_condition (operand2 exp) environment s))]
      [(eq? (operator exp) '!=)  (not (eq? (M_condition (left-operand exp) environment s)
                                           (M_condition (operand2 exp) environment s)))]
      [(eq? (operator exp) '<)   (<   (M_condition (left-operand exp) environment s) (M_condition (operand2 exp) environment s))]
      [(eq? (operator exp) '>)   (>   (M_condition (left-operand exp) environment s) (M_condition (operand2 exp) environment s))]
      [(eq? (operator exp) '<=)  (<=  (M_condition (left-operand exp) environment s) (M_condition (operand2 exp) environment s))]
      [(eq? (operator exp) '>=)  (>=  (M_condition (left-operand exp) environment s) (M_condition (operand2 exp) environment s))]

      [else                      (M_value exp environment s)])))

(define m-if-statement
  (lambda (exp environment s return break continue try catch finally)
    (cond
      [(null? exp)                          (error 'undefined "undefined expression")]

      [(M_condition (loop-condition exp) environment s) (M_state (loop-body exp) environment s
                                                     return break continue try catch finally)]

      [(null? (cdddr exp)) environment]

      [else                                 (M_state (else-statement exp) environment s
                                                     return break continue try catch finally)])))

(define m-while-loop
  (lambda (exp environment s return break continue try catch finally)
    (cond
      [(null? exp)
       (error 'undefined "undefined expression")]

      [(M_condition (loop-condition exp) environment s)
       (m-while-loop exp
                     (call/cc (lambda (k) (M_state (loop-body exp) environment s
                                                       return break k try catch finally)))
                     s
                     return break continue try catch finally)]

      [else environment])))

(define am-i-boolean
  (lambda (exp)
    (cond
      [(eq? (operator exp) '||)  #t]
      [(eq? (operator exp) '&&)  #t]
      [(eq? (operator exp) '!)   #t]
      [(eq? (operator exp) '==)  #t]
      [(eq? (operator exp) '!=)  #t]
      [(eq? (operator exp) '<)   #t]
      [(eq? (operator exp) '>)   #t]
      [(eq? (operator exp) '<=)  #t]
      [(eq? (operator exp) '>=)  #t]
      [else #f])))

(define m-return
  (lambda (exp environment s return finally)
    (cond
      [(eq?   exp #t)                       (return 'true)]
      [(eq?   exp #f)                       (return 'false)]
      [(and (pair? exp) (am-i-boolean exp)) (finally (m-return (M_condition exp environment s) environment s return finally))]
      [(and (pair? exp)
       (and (eq? (statement-type-id exp) 'funcall)
       (and (pair? (funcall-name exp))
            (eq? (car (funcall-name exp)) 'dot))))
                                            (return (m-dot-func (cadr (funcall-name exp)) (caddr (funcall-name exp)) (cddr exp) environment s return))]
      
      [(and (pair? exp) (and (eq? (statement-type-id exp) 'funcall) (null? (func-params exp))))
                                            (return (M_value (m-funcall (funcall-name exp) (cddr exp) return environment s) environment s))]

      
      [(and (pair? exp) (eq? (statement-type-id exp) 'funcall))
                                            (return (m-funcall (funcall-name exp) (func-params exp) return environment s))]

      [(pair? exp)                          (return (M_value exp environment s))]
      [(eq? (M_value exp environment s) #t)     (return 'true)]
      [(eq? (M_value exp environment s) #f)     (return 'false)]
      [else                                 (return (M_value exp environment s))])))

(define m-assign
  (lambda (assign environment s)
    (if (and (not (list (variable assign))) (not (locate-var (variable assign) (environment-body environment) environment s)))
        (error "use before declaration")
        (m-update (variable assign) (M_value (expression assign) environment s) environment s))))


(define m-var-dec
  (lambda (dec environment s)
    (cond
      [(local-locate (variable dec) (environment-body environment)) (error "redefining")]
      [(null? (assignment dec))        (m-add (variable dec) environment s)]
      [(eq? (is-new-instance dec) 'new) (m-instance-dec dec environment s)]
      [(and (pair? (expression dec)) (am-i-a-class-name (car (expression dec)) s))
                                       (m-update (variable dec)
                                                  (expression dec)
                                                  (m-add (variable dec) environment s) s)]

      
      [else                             (m-update (variable dec)
                                                 (M_value (expression dec) environment s)
                                                 (m-add (variable dec) environment s) s)])))

(define am-i-a-class-name
  (lambda (class-name s)
    (cond
      [(null? s) #f]
      [(equal? class-name (next-class s)) #t]
      [else (am-i-a-class-name class-name (next-part-classes s))])))

(define m-instance-dec
  (lambda (dec environment s)
    (m-update (variable dec) (m-lookup-class (instance-class-name dec) s) (m-add-global-var (variable dec) environment s) s)))


(define m-global-var-dec
  (lambda (dec environment s)
    (cond
      [(locate-global-var-simple (variable dec) (environment-body environment))  (error "redefining")]
      [(null? (assignment dec))              (m-add-global-var (variable dec) environment s)]
      [else                                  (m-update (variable dec)
                                                       (M_value (expression dec) environment s)
                                                       (m-add-global-var (variable dec) environment s) s)])))


(define m-lookup-var
  (lambda (var environment s)
    (m-lookup-var-nested var (environment-body environment) environment s)))


(define m-lookup-var-nested
  (lambda (var environment-s environment state)
    (cond
      [(null? environment-s)                       (error "use before declared")]
      [(null? (local environment-s))               (lookup-global-var var environment-s environment state)]
      [(null? (vars environment-s))                (m-lookup-var-nested var (nextlayer environment-s) environment state)]
      [(and (equal? var (nextvar environment-s)) (eq? "init" (unbox (nextval environment-s))))
                                               (error "use before assignment")]
      [(equal? var (nextvar environment-s))        (unbox (nextval environment-s))]
      [else                                    (m-lookup-var-nested var (next-part-vars environment-s) environment state)])))

 
(define lookup-global-var
  (lambda (var environment-s environment state)
    (cond
     [(and (empty-check-vars environment-s) (not (null? (environment-super environment))))
                                      (m-lookup-var var (m-lookup-class (car (environment-super environment)) state) state)]
     [(empty-check-vars environment-s)    (error "use before declared")]

     [(and (eq? var (global-nextvar environment-s)) (eq? "init" (unbox (global-nextval environment-s))))
                                      (error "use before assignment")]
     [(equal? var (global-nextvar environment-s)) (unbox (global-nextval environment-s))]
     [else                            (lookup-global-var var (global-nextpart-vars environment-s) environment state)])))

(define empty-check-vars
  (lambda (environment-s)
    (if (or (or (null? environment-s) (null? (global environment-s))) (null? (global-vars environment-s)))
        #t
        #f)))


(define m-lookup-func
  (lambda (var environment s)
    (m-lookup-func-nested var (environment-body environment) environment s)))


(define m-lookup-func-nested
  (lambda (func environment-s environment state)
    (cond
      [(null?  environment-s)                      (error "function not found")]
      [(null? (local  environment-s))              (lookup-global-func func environment-s environment state)]
      [(null? (funcs  environment-s))              (m-lookup-func-nested func (nextlayer  environment-s) environment state)]
      [(equal? func (nextfunc  environment-s))     (unbox (nextfunc-def  environment-s))]
      [else                                    (m-lookup-func-nested func (next-part-funcs  environment-s) environment state)])))


(define lookup-global-func
  (lambda (func environment-s environment state)
    (cond
     [(and (empty-check-funcs environment-s) (not (null? (environment-super environment))))
                                        (m-lookup-func func (m-lookup-class (car (environment-super environment)) state) state)]
     [(empty-check-funcs environment-s)     (error "function not found")]
     [(equal? func (global-nextfunc environment-s))
                                        (unbox (global-nextfunc-def environment-s))]
     [else                              (lookup-global-func func (global-nextpart-funcs environment-s) environment state)])))

(define empty-check-funcs
  (lambda (environment-s)
    (if (or (or (null? environment-s) (null? (global environment-s))) (null? (global-funcs environment-s)))
        #t
        #f)))

(define m-update
  (lambda (var update-val environment s)
    (list (environment-class-name environment) (environment-super environment) (m-update-nested var update-val (environment-body environment) environment s))))


(define m-update-nested
  (lambda (var update-val environment-s environment s)
    (cond
      [(null? environment-s)                          "error"]
      [(and (and (list? var) (eq? (car var) 'dot)) (eq? (cadr var) 'this))
                                                  (list (local environment-s) (global-update (caddr var) update-val (global environment-s)))]
      [(not (locate-var var environment-s environment s)) "error"]
      [(local-locate-var var environment-s)           (list (local-update var update-val (local environment-s)) (global environment-s))]
      [else                                       (list (local environment-s) (global-update var update-val (global environment-s)))])))

(define local-update
  (lambda (var update-val s)
    (cond
      [(null? s)      "error"]
      [(local-layer-locate var (top-layer s))
                      (cons (local-toplayer-update var update-val (top-layer s)
                                                   (lambda (v1 v2) (list (list v1 v2) (local-funcs s))))
                            (rest-of s))]
      [else           (cons (top-layer s) (local-update var update-val (cdr s)))])))


(define global-update
  (lambda (var update-val s)
    (cond
      [(null? s)                         "error"]
      [(not (local-layer-locate var s))  "error"]
      [else                              (local-toplayer-update var update-val s
                                                                (lambda (v1 v2) (list (list v1 v2)
                                                                                      (s-funcs s))))])))

(define local-toplayer-update
  (lambda (var update-val s return)
    (if (equal? var (s-nextvar s))
        (return (s-vars s) (begin  (set-box! (s-nextval s) update-val)
                                   (cons (s-nextval s) (rest-of (s-vals s)))))
        (local-toplayer-update var update-val  (s-next-part-vars s)
                               (lambda (v1 v2) (return (cons (s-nextvar s) v1)
                                                       (cons (s-nextval s) v2)))))))


(define m-add
  (lambda (var environment s)
    (list (environment-class-name environment) (environment-super environment) (m-add-nested var (environment-body environment)))))

(define m-add-nested
  (lambda (var s)
     (list (cons (list (list (cons  var (vars s))
                             (cons (box "init") (vals s))) (func-layer s))
                 (cdr (local s)))
           (global s))))

(define m-add-local-func
  (lambda (func func-environment class-environment s)
    (list (environment-class-name class-environment) (environment-super class-environment) (m-add-local-func-nested func func-environment (environment-body class-environment) s))))



(define m-add-local-func-nested
  (lambda (func func-environment class-environment s)
    (list (cons (list (var-layer class-environment) (list (cons func (funcs class-environment))
                                          (cons (box func-environment) (func-defs class-environment))))
                (cdr (local class-environment)))
          (global class-environment))))

(define m-add-global-var
  (lambda (var environment s)
    (list (environment-class-name environment) (environment-super environment) (m-add-global-var-nested var (environment-body environment)))))

(define m-add-global-var-nested
  (lambda (var s)
    (list (local s) (list (list (cons var (global-vars s))
                                (cons (box "init") (global-vals s)))
                          (global-func-layer s)))))

(define m-add-global-func
  (lambda (func func-environment class-environment)
    (list (environment-class-name class-environment) (environment-super class-environment) (m-add-global-func-nested  func func-environment (environment-body class-environment)))))

(define m-add-global-func-nested
  (lambda (func environment s)
    (list (local s) (list (global-var-layer s)
                          (list (cons func (global-funcs s))
                                (cons (box environment) (global-func-defs s)))))))


(define local-locate
  (lambda (var s)
    (cond
      [(null? s)             #f]
      [(null? (vars s))      #f]
      [(eq? var (nextvar s)) #t]
      [else                  (local-locate var (next-part-vars s))])))


(define local-layer-locate
  (lambda (var s)
    (cond
      [(null? s)               #f]
      [(null? (s-vars s))      #f]
      [(eq? var (s-nextvar s)) #t]
      [else                    (local-layer-locate var (s-next-part-vars s))])))


(define locate-var
  (lambda (var environment-s environment state)
    (cond
      [(null? environment-s)             #f]
      [(eq? (local environment-s) '())   (locate-global-var var environment-s environment state)]
      [(null? (vars environment-s))      (locate-var var (nextlayer environment-s) environment state)]
      [(eq? var (nextvar environment-s)) #t]
      [else                  (locate-var var (next-part-vars environment-s) environment state)])))


(define locate-global-var
  (lambda (var environment-s environment state)
    (cond
      [(and (empty-check-vars environment-s) (not (null? (environment-super environment))))
       (locate-var var (environment-body (m-lookup-class (car (environment-super environment)) state))
                   (m-lookup-class (car (environment-super environment)) state) state)]
      [(empty-check-vars environment-s)         #f]
      [(eq? var (global-nextvar environment-s)) #t]
      [else                         (locate-global-var var (global-nextpart-vars environment-s) environment state)])))

(define locate-global-var-simple
  (lambda (var environment-s)
    (cond
      [(empty-check-vars environment-s)         #f]
      [(eq? var (global-nextvar environment-s)) #t]
      [else                         (locate-global-var-simple var (global-nextpart-vars environment-s))])))


(define local-locate-var
   (lambda (var s)
    (cond
      [(null? s)             #f]
      [(null? (local s))     #f]
      [(null? (vars s))      (local-locate-var var (nextlayer s))]
      [(eq? var (nextvar s)) #t]
      [else                  (local-locate-var var (next-part-vars s))])))


(define locate-func
  (lambda (func s)
    (cond
      [(null? s)               #f]
      [(eq? (local s) '())     (locate-global-func func s)]
      [(null? (funcs s))       (locate-func func (nextlayer s))]
      [(eq? func (nextfunc s)) #t]
      [else                    (locate-func func (next-part-funcs s))])))

(define locate-global-func
  (lambda (func s)
    (cond
      [(null? s)                      #f]
      [(null? (global s))             #f]
      [(null? (global-funcs s))       #f]
      [(eq? func (global-nextfunc s)) #t]
      [else                           (locate-global-func func (global-nextpart-funcs s))])))

(define get-instance
  (lambda (name environment s)
    (m-lookup-var name environment s)))

(define m-dot-func
  (lambda (instance func-name params environment s return)
    (cond
      [(and (list? instance) (eq? (car instance) 'new))
                   (m-funcall func-name (get-params-from-big-boy params environment s) return (m-lookup-class (cadr instance) s) s)]

      [(eq? instance 'super)
                   (m-funcall func-name (get-params-from-big-boy params environment s) return (m-lookup-class (car (environment-super environment)) s) s)]
      
      [else
                   (m-funcall func-name (get-params-from-big-boy params environment s) return (get-instance instance environment s) s)])))
      

(define get-params-from-big-boy
  (lambda (params environment s)
    (cond
      [(null? params) '()]
      [(and (list? params) (list? first-param))
                      (cons (m-dot-value (first-param-dot-instance params) (first-param-dot-variable params) environment s)
                            (get-params-from-big-boy (other-params params) environment s))]
      [(list? params) (cons (M_value (first-param params) environment s) (get-params-from-big-boy (other-params params) environment s))]
      [else           (M_value params environment s)])))



(define m-dot-value
  (lambda (instance variable environment s)
    (cond
      [(and (list? instance) (eq? (car instance) 'new))
            (m-lookup-var variable (m-lookup-class (cadr instance) s) s)]
      [else (m-lookup-var variable (get-instance instance environment s)  s)])))
    

(define m-lookup-class
  (lambda (class-name s)
    (cond
      [(null? s) (error "class does not exist")]
      [(equal? class-name (next-class s)) (new-environment (next s))]
      [else (m-lookup-class class-name (next-part-classes s))])))

(define m-new-environment
  (lambda (class-name class-super environment)
    (list (class-name class-super environment))))

(define m-lookup-super-class
  (lambda (class-name s)
    (cond
      [(null? s) (error "class does not exist")]
      [(equal? class-name (next-class s)) (next-extends s)]
      [else (m-lookup-super-class class-name (next-part-classes s))])))

(define m-add-class
 (lambda (class-dec s)
   (cons (generate-environment (class-body class-dec) (list (class-name class-dec) (class-extends class-dec) empty-state) s) s)))

(define generate-environment
  (lambda (body environment s)
    (cond
      [(null? body) environment]
      [(equal? 'var (first (next body))) (generate-environment (cdr body) (m-global-var-dec (next body) environment s) s)]
      [(equal? 'function (first (next body)))
       (generate-environment (cdr body) (m-add-global-func  (full-func (next body)) (list (append (list (func-name (next body)))(list (func-body (next body))))) environment) s)]
      [(equal? 'static-function (first (next body)))
       (generate-environment (cdr body) (m-add-global-func  (full-func (next body)) (list (append (list (func-name (next body)))(list (func-body (next body))))) environment) s)]
      [else (generate-environment (cdr body) environment s)])))






(define new-environment
  (lambda (environment)
     (list (environment-class-name environment) (environment-super environment) (new-environment-body (environment-body environment)))))

(define new-environment-body
  (lambda (environment-body)
    (list (new-local (local environment-body)) (new-layer-gen (global environment-body)))))

(define new-local
  (lambda (local)
    (cond
     [(null? local) '()]
     [else (cons (new-layer-gen (car local)) (new-local (next-part-list local)))])))

(define new-layer-gen
  (lambda (layer)
    (list (list (s-vars layer) (handle-boxes (s-vals layer))) (list (s-just-funcs layer) (handle-boxes (s-func-defs layer))))))


(define handle-boxes
  (lambda (list)
    (box-all (unbox-all list))))

(define unbox-all
  (lambda (list)
    (if (null? list)
        '()
        (cons (unbox (next-in-list list)) (unbox-all (next-part-list list))))))

(define box-all
  (lambda (list)
    (if (null? list)
        '()
        (cons (box (next-in-list list)) (box-all (next-part-list list))))))

(define next-in-list car)
(define next-part-list cdr)
(define statement-type-id car) 
(define statement-body cadr)   

(define else-statement cadddr) 
(define loop-condition cadr)
(define loop-body caddr)

(define left-operand cadr)
(define operator car)
(define operand2 caddr)
(define operand2-exists cddr)
(define operand1 cadr)
(define dot-variable-name caddr)

(define assignment cddr)
(define is-new-instance
  (lambda (dec)
    (if (list? (expression dec))
        (caaddr dec)
        (expression dec))))

(define instance-class-name (lambda (v) (cadar (assignment v))))
(define variable cadr)
(define expression caddr)

(define try-body cadr)
(define catch-statement caddr)
(define second-identifier caaddr) 
(define second-body cdaddr) 
(define third-statement cadddr)
(define third-identifier (lambda (s) (car (third-statement s))))
(define third-body (lambda (s) (cadr (third-statement s))))
(define catch-body cadr)
(define catch-var-name caar)

(define dot-var-name  cadadr)
(define dot-func-name (lambda (s) (car (cddadr s))))

(define first-val car)

(define func-name caddr)
(define func-body cdddr)
(define full-func cadr)
(define main-body cadddr)
(define func-params cddr)
(define funcall-name cadr)
(define no-params '())

(define func-formal-params caar)
(define func-call-body caadar)

(define vars caaaar) 
(define vals (lambda (s) (car (cdaaar s)))) 
(define nextvar (lambda (s) (car (vars s)))) 
(define nextval (lambda (s) (car (vals s)))) 
(define local-layer caar) 
(define var-layer  caaar)
(define func-layer cadaar)
(define func-layer2 cadar)
(define global cadr) 
(define local car) 
(define toplayer caar)

(define first-param  car)
(define other-params cdr)
(define first-param-dot-instance cadar)
(define first-param-dot-variable caddar)

(define next-local-layer 
  (lambda (s)
        (list (cdr (local s)) (global s))))

(define nextlayer next-local-layer)

(define layer car) 

(define next-part-vars
  (lambda (s)
    (list (cons (list (list (cdr (vars s)) (cdr (vals s))) (func-layer s)) (cdr (local s))) (global s)))) 

(define next-part-funcs
  (lambda (s)
    (list (cons (list (var-layer s) (list (cdr (funcs s)) (cdr (func-defs s)))) (cdr (local s))) (global s))))

(define nextfunc (lambda (s) (caar (func-layer s))))
(define nextfunc-def (lambda (s) (caadr (func-layer s))))
(define funcs (lambda (s) (car (func-layer s)))) 
(define func-defs (lambda (s) (cadr (func-layer s)))) 
(define global-var-layer (lambda (s) (car (global s))))
(define global-func-layer (lambda (s) (cadr (global s))))
(define global-vars (lambda (s) (car (global-var-layer s))))
(define global-vals (lambda (s) (cadr (global-var-layer s))))
(define global-funcs (lambda (s) (car (global-func-layer s))))
(define global-func-defs (lambda (s) (cadr (global-func-layer s))))
(define global-nextvar (lambda (s) (car (global-vars s))))
(define global-nextval (lambda (s) (car (global-vals s))))
(define global-nextfunc (lambda (s) (car (global-funcs s))))
(define global-nextfunc-def (lambda (s) (car (global-func-defs s))))
(define global-nextpart-vars
  (lambda (s)
    (list (local s) (cons (list (cdr (global-vars s)) (cdr (global-vals s))) (cdr (global s))))))
(define global-nextpart-funcs
  (lambda (s)
    (list (local s) (list (global-var-layer s) (list (cdr (global-funcs s)) (cdr (global-func-defs s)))))))

(define rest-of cdr)
(define b-global-vars (lambda (s) (caar (global s))))
(define b-global-nextval (lambda (s) (cadar (global s))))

(define s-nextvar caaar)
(define s-nextval caadar)
(define s-varlayer car)
(define s-vars caar)
(define s-vals cadar)
(define s-funcs cadr)
(define s-just-funcs caadr)
(define s-func-defs cadadr)
(define s-next-part-vars
  (lambda (s)
    (list (list (cdr (s-vars s)) (cdr (s-vals s))) (cadr s))))

(define top-layer car)

(define local-funcs cadar)
(define next-part-local cdr)

(define new-layer '((()())(()())))
(define empty-state '((((()())(()())))((()())(()()))))
(define empty-list '())
(define class-adding-state '(()(()())))
(define b '((((()())(()())))((()())(()()))))
(define firstExpression car)
(define expressionRemainder cdr)

(define next-full-class-environment car)
(define next-class caar)
(define next-extends cadar)
(define next-environment caddar)
(define next-part-classes cdr)
(define c1 '(class B (extends A)  body))
(define c2 '(class A () body))

(define class-name cadr)
(define class-extends
  (lambda (class-environment)
    (if (null? (caddr class-environment))
        '()
      (cdaddr class-environment))))
(define class-body cadddr)

(define environment-super cadr)


(define environment-class-name car)
(define environment-body caddr)
(define next car)
(define first car)

(define interpreter
  (lambda (inputfile callcc classmain)
    (let* [(s (M_state_function (parse-tree inputfile) empty-list empty-list
                                                                  callcc
                                                                  (lambda (v) v)
                                                                  (lambda (v) v)
                                                                  (lambda (v) v)
                                                                  (lambda (v) v)
                                                                  (lambda (v) v)))]
       (m-funcall 'main no-params callcc
                  (m-lookup-class classmain s) s))))


(define interpret
  (lambda (inputfile classmain)
    (call/cc
     (lambda (k)
       (interpreter inputfile k (string->symbol classmain))))))


; -----
; Tests
; -----
(displayln "Part 4 Tests")
(display "Test 01 (Expected output is 15)       Actual Output -> ")
(displayln (interpret "p4_test1.txt" "A"))
(display "Test 02 (Expected output is 12)       Actual Output -> ")
(displayln (interpret "p4_test2.txt" "A"))
(display "Test 03 (Expected output is 125)      Actual Output -> ")
(displayln (interpret "p4_test3.txt" "A"))
(display "Test 04 (Expected output is 36)       Actual Output -> ")
(displayln (interpret "p4_test4.txt" "A"))
(display "Test 05 (Expected output is 54)       Actual Output -> ")
(displayln (interpret "p4_test5.txt" "A"))
(display "Test 06 (Expected output is 110)      Actual Output -> ")
(displayln (interpret "p4_test6.txt" "A"))

(display "Test 07 (Expected output is 26)       Actual Output -> ")
(displayln (interpret "p4_test7.txt" "C"))
;(display "Test 08 (Expected output is 117)      Actual Output -> ")
;(displayln (interpret "p4_test8.txt" "Square"))
;(display "Test 09 (Expected output is 32)       Actual Output -> ")
;(displayln (interpret "p4_test9.txt" "Square"))
;(display "Test 10 (Expected output is 15)       Actual Output -> ")
;(displayln (interpret "p4_test10.txt" "List"))
;(display "Test 11 (Expected output is 123456)   Actual Output -> ")
;(displayln (interpret "p4_test11.txt" "List"))
;(display "Test 12 (Expected output is 5285)     Actual Output -> ")
;(displayln (interpret "p4_test12.txt" "List"))
;(display "Test 13 (Expected output is -716)     Actual Output -> ")
;(displayln (interpret "p4_test13.txt" "C"))
(displayln "End of tests from Part 4")
(displayln "")
