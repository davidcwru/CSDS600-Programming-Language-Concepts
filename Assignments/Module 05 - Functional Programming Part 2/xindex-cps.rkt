#lang racket
(define (xindex-cps atom lst k)
  ;; Helper function to find the index of an atom in a list
  (define (find-atom-index lst atom)
    (let loop ((lst lst) (index 1))
      (cond
        ((null? lst) #f)
        ((equal? (car lst) atom) index)
        (else (loop (cdr lst) (+ index 1))))))

  ;; Helper function to process a list
  (define (process-list lst k)
    (cond
      ((null? lst) (k '()))                     ; If the list is empty, invoke the continuation with an empty list
      ((list? (car lst))                        ; If the car of the list is a list
       (let ((idx (find-atom-index (car lst) atom))) ; Find index of atom in sublist
         (if idx                                    ; If atom found
             (process-list (cdr lst)             ; Recursively process the rest of the list
                           (lambda (result)      ; Pass continuation
                             (k (cons (list idx) result))))  ; Cons the index with the result and invoke continuation
             (process-list (car lst)             ; Otherwise, process the sublist
                           (lambda (processed-sublist)
                             (process-list (cdr lst)  ; Recursively process the rest of the list
                                           (lambda (rest-result)
                                             (k (cons processed-sublist rest-result))))))))) ; Cons the processed sublist with the result and invoke continuation
      (else                                       ; If the car is not a list
       (process-list (cdr lst)                     ; Recursively process the rest of the list
                     (lambda (result)              ; Pass continuation
                       (k (cons (car lst) result)))))))  ; Cons the car with the result and invoke continuation

  (let ((idx (find-atom-index lst atom)))      ; Find index of atom in the main list
    (if idx                                       ; If atom found
        (k (list idx))                            ; Invoke continuation with the index as a list
        (process-list lst k))))                   ; Otherwise, process the list

(xindex-cps 'x '((a b c) (d e x f) (((g h) i) j) k (((l m x o)))) (lambda (res) res))
; '((a b c) (3) (((g h) i) j) k (((3))))
(newline)
(xindex-cps 'x '((a b c) (d e x g) (((h i) x) j) x k (((l m x o)))) (lambda (res) res))
;'(4)
(newline)
(xindex-cps 'x '((a b c) (d e x g) (((h i) x) j k ((l m x o)))) (lambda (res) res))
;'((a b c) (3) ((2) j k ((3))))