#lang racket

; ------------------------------------------------------------------------------
; 1. dotproduct takes a two vectors (lists of numbers) and computes the dot
; product of the vectors. If one list is longer than the other, you can ignore
; the extra numbers of the longer list.
;
; lis1[0] * lis2[0] + ... + lis1[n-1] * lis2[n-1] + lis1[n] * lis2[n]
;
; (dotproduct-cps '(1 2 3) '(-2 1 5) (lambda (result) result))
; ------------------------------------------------------------------------------

(define dotproduct-cps
  (lambda (lis1 lis2 cont)
    (cond
      ((or (null? lis1) (null? lis2)) 
       (cont 0))
      (else 

        (dotproduct-cps (cdr lis1) (cdr lis2) 
                       (lambda (tail-dot)
                         (cont (+ (* (car lis1) (car lis2)) tail-dot))
                       ) ; end lambda
        ) ; end dotproduct call
      ) ; end else
    ) ; end cond
  ) ; end lambda
) ; end define


; ------------------------------------------------------------------------------
; 2. removesubsequence takes two lists of atoms. The first list is a subsequence
; of the second list. The method should return the second list with the first
; occurence of the subsequence removed. So, if the first list is '(a b c), the
; first a if the second list is removed, the first b that appears after the
; removed a is removed, and the first c that appears after the removed b is
; removed.
;
; (removesubsequence-cps '(1 3 5) '(0 1 2 3 4 5 6)     (lambda (result) result))
; (removesubsequence-cps '(1 3 5) '(5 4 3 2 1 2 3 4 5) (lambda (result) result))
; (removesubsequence-cps '(a b c) '(d b c a c b a b c) (lambda (result) result))
; ------------------------------------------------------------------------------

(define removesubsequence-cps
  (lambda (subseq lis cont)
    ; Define the CPS version of remove-helper
    (define (remove-helper-cps subseq lis cont)
      (cond
        ((or (null? subseq) (null? lis)) 
         (cont lis))
        ((equal? (car subseq) (car lis))
         (remove-helper-cps (cdr subseq) (cdr lis) cont))
        (else

         (remove-helper-cps subseq (cdr lis) 
                            (lambda (result) 
                              (cont (cons (car lis) result)))))
      ) ; end cond
    ) ; end define

    (remove-helper-cps subseq lis cont)
  ) ; end lambda
) ; end define

; ------------------------------------------------------------------------------
; 3. squareroot takes two numbers, a value and an iteration. The iteration will
; be an integer greater than or equal to 0. The method will compute the
; squareroot of the value using iteration rounds of Newton's method, starting
; with an initial value equal to the input value.
;
; Newton's method is new = old - ((old * old) - value) / (2 * old)
;
; (squareroot-cps 5.0 0 (lambda (result) result))
; (squareroot-cps 5.0 1 (lambda (result) result))
; (squareroot-cps 5.0 5 (lambda (result) result))
; (squareroot-cps 5 5   (lambda (result) result))
; ------------------------------------------------------------------------------

(define squareroot-cps
  (lambda (value iteration cont)
    (define (sqrt-iter-cps old value iteration cont)
      (cond
        ((= iteration 0) 
         (cont old))
        (else
         ; Continue the iteration process with updated values and a tail call
         (sqrt-iter-cps
          (- old (/ (- (* old old) value) (* 2 old)))
          value 
          (- iteration 1)
          cont))
      )) ; end cond
    ; Start the iteration process with initial values and continuation
    (sqrt-iter-cps value value iteration cont)
  ) ; end lambda
) ; end define


; ------------------------------------------------------------------------------
; 4. numatoms* takes a nested list and returns the number of atoms (not
; including the empty list) in the list
;
; (numatoms*-cps '(a (a (a b) c) a) (lambda (result) result))
; ------------------------------------------------------------------------------

(define numatoms*-cps
  (lambda (lst cont)
    (define (count-atoms lst cont)
      (cond
        ((null? lst) 
         (cont 0)) ; Base case: empty list, return 0 via continuation
        ((not (pair? lst)) 
         (cont 1)) ; Base case: single atom, return 1 via continuation
        (else 
         ; Recursive case: split the work between the first element and the rest
         (count-atoms (car lst) 
                      (lambda (count-car)
                        (count-atoms (cdr lst) 
                                     (lambda (count-cdr)
                                       (cont (+ count-car count-cdr)))))))))
    ; Start counting with the initial list and continuation
    (count-atoms lst cont)
  ) ; end lambda
) ; end define


; ------------------------------------------------------------------------------
; 5. reverse* takes a nested list and reverses the contents of the list and all
; nested lists.
;
; (reverse*-cps '(a b (c (d e ((f) g)) h)) (lambda (result) result))
; ------------------------------------------------------------------------------

(define reverse*-cps
  (lambda (lis cont)
    (cond
      ((null? lis) (cont '()))
      (else
        ; First, reverse the cdr of the list with a continuation
        (reverse*-cps (cdr lis) 
                      (lambda (reversed-cdr)
                        ; Check if the car of the original list is a list itself
                        (if (list? (car lis))
                            ; If so, reverse it as well and then append
                            (reverse*-cps (car lis)
                                          (lambda (reversed-car)
                                            (cont (append reversed-cdr (list reversed-car)))))
                            ; If not, simply append the car to the reversed cdr
                            (cont (append reversed-cdr (list (car lis))))))))
    ) ; end cond
  ) ;end lambda
) ; end define


; ------------------------------------------------------------------------------
; 6. vectormult takes a row vector (a list of numbers) and matrix (a list of
; lists of numbers) and multiplies the vector times the matrix. The result is a
; vector where the ith element of the result is the dotproduct of the input
; vector and the ith column of the matrix. You can assume that the length of the
; vector matches the number of rows of the matrix.
;
; (vectormult-cps '(1 2 -1) '((0 2 3) (1 2 0) (1 0 3)) (lambda (result) result))
; ------------------------------------------------------------------------------

(define vectormult-cps
  (lambda (vec mat cont)

    ; Define transpose in CPS
    (define (transpose-cps m cont)
      (if (null? (car m))
          (cont '())
          (build-row-cps m
                         (lambda (row)
                           (transpose-cps (remove-heads-cps m (lambda (x) x))
                                          (lambda (rest) (cont (cons row rest))))))))

    ; Define build-row in CPS
    (define (build-row-cps m cont)
      (if (null? m)
          (cont '())
          (build-row-cps (cdr m)
                         (lambda (rest) (cont (cons (car (car m)) rest))))))

    ; Define remove-heads in CPS
    (define (remove-heads-cps m cont)
      (if (null? m)
          (cont '())
          (remove-heads-cps (cdr m)
                            (lambda (rest) (cont (cons (cdr (car m)) rest))))))

    ; Define mult-helper in CPS
    (define (mult-helper-cps v m cont)
      (if (null? m)
          (cont '())
          (dotproduct-cps v (car m)
                           (lambda (dp)
                             (mult-helper-cps v (cdr m)
                                              (lambda (rest) (cont (cons dp rest))))))))

    ; Transpose matrix and multiply
    (transpose-cps mat 
                   (lambda (transposed)
                     (mult-helper-cps vec transposed cont)))
  ) ; end lambda
) ; end define


; ------------------------------------------------------------------------------
; 7. matrixmultiply takes two matrices (a list of lists of numbers) and
; multiplies them. You can assume the number of columns of the first matrix is
; equal to the number of rows of the second matrix. in the same sublist
;
; (matrixmultiply-cps '((1 0 1) (1 1 1) (0 1 1)) '((2 3 4) (-1 1 2) (3 1 -2)) (lambda (result) result))
; ------------------------------------------------------------------------------

;
; I give up trying to make this code legible. This language is simply not
; conducive to readable code.
;

(define (transpose-cps mat cont)
  (define (first-elements-cps mat cont)
    (if (null? mat)
        (cont '())
        (first-elements-cps (cdr mat)
          (lambda (rest) (cont (cons (car (car mat)) rest))))))
  
  (define (remove-first-elements-cps mat cont)
    (if (null? mat)
        (cont '())
        (remove-first-elements-cps (cdr mat)
          (lambda (rest) (cont (cons (cdr (car mat)) rest))))))
  
  (if (null? (car mat))
      (cont '())
      (first-elements-cps mat
        (lambda (firsts)
          (remove-first-elements-cps mat
            (lambda (rests)
              (transpose-cps rests
                (lambda (transposed-rests)
                  (cont (cons firsts transposed-rests)))))))))
)

(define matrixmultiply-cps
  (lambda (mat1 mat2 cont)
    (transpose-cps mat2
      (lambda (transposed-mat2)
        (define (multiply-row-cps row mat cont)
          (if (null? mat)
            (cont '())
            (dotproduct-cps row (car mat)
            (lambda (result)
              (multiply-row-cps row (cdr mat)
                (lambda (rest) (cont (cons result rest))))))))
                  (define (multiply-matrix-cps mat cont)
                    (if (null? mat)
                      (cont '())
                        (multiply-row-cps (car mat) transposed-mat2
                          (lambda (result)
                            (multiply-matrix-cps (cdr mat)
                              (lambda (rest) (cont (cons result rest))))))))
                     
                     (multiply-matrix-cps mat1 cont))
    )
  ) ; end lambda
) ; end define


; ------------------------------------------------------------------------------
; 8. removesubsequence* takes a list of atoms and a general list. The first list
; is a subsequence of the second list. The method should return the second list
; with the first occurence of the subsequence removed. So, if the first list is
; '(a b c), the first a if the second list is removed, the first b that appears
; after the removed a is removed, and the first c that appears after the removed
; b is removed - no matter how deep the atoms are nested.
;
; (removesubsequence*-cps '(a b) '(w (x b) ((a) ((y z))) b) (lambda (result found) result))
; ------------------------------------------------------------------------------

(define removesubsequence*-cps
  (lambda (subseq list cont)
    (define (remove-cps subseq list cont)
      (cond
        ((null? list) (cont '() subseq))  ; End of list
        ((null? subseq) (cont list subseq))  ; End of subsequence
        (else
          (let ((head (car list))
                (tail (cdr list)))
            (if (list? head)
                ; If head is a list, process it recursively
                (remove-cps subseq head
                            (lambda (new-head new-subseq)
                              ; Continue with the possibly updated subseq and the rest of the list
                              (remove-cps new-subseq tail
                                          (lambda (new-tail final-subseq)
                                            (cont (cons new-head new-tail) final-subseq)))))
                ; Else, check if head matches the first element of the subseq
                (if (and (not (null? subseq)) (equal? head (car subseq)))
                    ; If it matches, remove this element and move to the next element of the subseq
                    (remove-cps (cdr subseq) tail cont)
                    ; If it does not match, keep the head and continue with the same subseq
                    (remove-cps subseq tail
                                (lambda (new-tail final-subseq)
                                  (cont (cons head new-tail) final-subseq)))))))))
    (remove-cps subseq list (lambda (result final-subseq) (cont result final-subseq)))
  ) ; end lambda
) ; end define


; ------------------------------------------------------------------------------
; 9.
;
; (suffix-cps 'x '(a b c) (lambda (result) result))
; (suffix-cps 'x '(a b x c d x e f) (lambda (result) result))
;
; ------------------------------------------------------------------------------

(define suffix-cps
  (lambda (atom lis return)
    (letrec ((process
      (lambda (lis collected)
        (cond
          ((null? lis) (return collected))
          ((eq? atom (car lis))
          (call/cc 
          (lambda (k)
            (process (cdr lis) '()))))
            (else
            (call/cc
            (lambda (k)
              (process (cdr lis)
              (append collected (list (car lis)))))))))))
      (process lis '())
   ) ; end letrec
  ) ; end lambda
) ; end define


; ------------------------------------------------------------------------------
; 10. xindex takes an atom and a list containing sublists. The output list
; should be the same as the input list except that any sublist (including the
; main list) that contains the given atom should be emptied of all contents
; (atoms, lists, etc.), and instead, the only content of that sublist should be
; the index of the first occurrence of the atom in that list.
;
; (xindex-cps 'x '((a b c) (d e x f) (((g h) i) j) k (((l m x o)))) (lambda (result) result))
; (xindex-cps 'x '((a b c) (d e x g) (((h i) x) j) x k (((l m x o)))) (lambda (result) result))
; (xindex-cps 'x '((a b c) (d e x g) (((h i) x) j k ((l m x o)))) (lambda (result) result))
; ------------------------------------------------------------------------------

(define xindex-cps
  (lambda (atom lst k)

  (define (find-atom-index lst atom)
    (let loop ((lst lst) (index 1))
      (cond
        ((null? lst) #f)
        ((equal? (car lst) atom) index)
        (else (loop (cdr lst) (+ index 1))))))


  (define (process-list lst k)
    (cond
      ((null? lst) (k '()))
      ((list? (car lst))
       (let ((idx (find-atom-index (car lst) atom)))
         (if idx ; found the atom
             (process-list (cdr lst)
                           (lambda (result)
                             (k (cons (list idx) result))))
             (process-list (car lst)
                           (lambda (processed-sublist)
                             (process-list (cdr lst)
                                           (lambda (rest-result)
                                             (k (cons processed-sublist rest-result)))))))))
      (else ; list does not have the car
       (process-list (cdr lst)
                     (lambda (result)
                       (k (cons (car lst) result)))))))

  (let ((idx (find-atom-index lst atom)))
    (if idx
        (k (list idx))
        (process-list lst k)))
  ) ; end lambda
) ; end define





(display "Problem 1, Test 1: ")
(displayln (dotproduct-cps '(1 2 3) '(-2 1 5) (lambda (result) result)))
(displayln "")

(display "Problem 2, Test 1: ")
(displayln (removesubsequence-cps '(1 3 5) '(0 1 2 3 4 5 6)     (lambda (result) result)))
(display "Problem 2, Test 2: ")
(displayln (removesubsequence-cps '(1 3 5) '(5 4 3 2 1 2 3 4 5) (lambda (result) result)))
(display "Problem 2, Test 3: ")
(displayln (removesubsequence-cps '(a b c) '(d b c a c b a b c) (lambda (result) result)))
(displayln "")

(display "Problem 3, Test 1: ")
(displayln (squareroot-cps 5.0 0 (lambda (result) result)))
(display "Problem 3, Test 2: ")
(displayln (squareroot-cps 5.0 1 (lambda (result) result)))
(display "Problem 3, Test 3: ")
(displayln (squareroot-cps 5.0 5 (lambda (result) result)))
(display "Problem 3, Test 4: ")
(squareroot-cps 5 5   (lambda (result) result))
(displayln "")

(display "Problem 4, Test 1: ")
(displayln (numatoms*-cps '(a (a (a b) c) a) (lambda (result) result)))
(displayln "")

(display "Problem 5, Test 1: ")
(displayln (reverse*-cps '(a b (c (d e ((f) g)) h)) (lambda (result) result)))
(displayln "")

(display "Problem 6, Test 1: ")
(displayln (vectormult-cps '(1 2 -1) '((0 2 3) (1 2 0) (1 0 3)) (lambda (result) result)))
(displayln "")

(display "Problem 7, Test 1: ")
(displayln (matrixmultiply-cps '((1 0 1) (1 1 1) (0 1 1)) '((2 3 4) (-1 1 2) (3 1 -2)) (lambda (result) result)))
(displayln "")

(display "Problem 8, Test 1: ")
(displayln (removesubsequence*-cps '(a b) '(w (x b) ((a) ((y z))) b) (lambda (result found) result)))
(displayln "")

(display "Problem 9, Test 1: ")
(displayln (suffix-cps 'x '(a b c) (lambda (result) result)))
(display "Problem 9, Test 1: ")
(displayln (suffix-cps 'x '(a b x c d x e f) (lambda (result) result)))
(displayln "")

(display "Problem 10, Test 1: ")
(displayln (xindex-cps 'x '((a b c) (d e x f) (((g h) i) j) k (((l m x o)))) (lambda (result) result)))
(display "Problem 10, Test 2: ")
(displayln (xindex-cps 'x '((a b c) (d e x g) (((h i) x) j) x k (((l m x o)))) (lambda (result) result)))
(display "Problem 10, Test 3: ")
(displayln (xindex-cps 'x '((a b c) (d e x g) (((h i) x) j k ((l m x o)))) (lambda (result) result)))
