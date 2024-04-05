; David Courtney - 3643806
;
; Assignment Instructions
; ------------------------------------------------------------------------------
; You may write any helper functions you need, and you may use functions created
; for one problem to solve another.
;
; Please do not use built in Scheme functions except the ones (and variants of
; them) we used in class: car, cdr (and all their variants), cons, null?, pair?,
; list?, number?, append, =, eq?, zero?, if, cond, and all the standard
; arithmetic and logic functions.
;
; Do not nest cond statements. Nor have more than two if statements nested
; inside each other. Instead, rearrange your logic so that you can write your
; function with a single cond of multiple cases.
; ------------------------------------------------------------------------------

#lang racket

; ------------------------------------------------------------------------------
; 1. inorder? takes a list of numbers and returns #t if the numbers are in
; non-decreasing order
; ------------------------------------------------------------------------------

(define inorder?
  (lambda (lis)
    (cond

      ( ( or (null? lis) (null? (cdr lis)) ) #t )
      ( (<= (car lis) (car (cdr lis))) (inorder? (cdr lis)))
      ( else #f )

    ) ; end cond
  ) ; end lambda
) ; end define

; ------------------------------------------------------------------------------
; 2. dotproduct takes a two vectors (lists of numbers) and computes the dot
; product of the vectors. If one list is longer than the other, you can ignore
; the extra numbers of the longer list.
;
; lis1[0] * lis2[0] + ... + lis1[n-1] * lis2[n-1] + lis1[n] * lis2[n]
; ------------------------------------------------------------------------------

(define dotproduct
  (lambda (lis1 lis2)
    (cond

      ( ( or (null? lis1) (null? lis2) ) 0 )
      (else ( + ( * (car lis1) (car lis2)) (dotproduct (cdr lis1) (cdr lis2))))

    ) ; end cond
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
; You do not have to make your solution efficient. Later, we will look at how to
; write efficient functional code.
; ------------------------------------------------------------------------------

(define squareroot
  (lambda (value iteration)
    (define (sqrt-iter old value iteration)
      (cond

        ((= iteration 0) old)
        (else
          (sqrt-iter
            (- old (/ (- (* old old) value) (* 2 old)))
            value (- iteration 1)
          )
        ) ; end else

      ) ; end cond
    ) ; end define sqrt-iter

    (sqrt-iter value value iteration)

  ) ; end lambda
) ; end define squareroot

; ------------------------------------------------------------------------------
; 4. removesubsequence takes two lists of atoms. The first list is a subsequence
; of the second list. The method should return the second list with the first
; occurence of the subsequence removed. So, if the first list is '(a b c), the
; first a if the second list is removed, the first b that appears after the
; removed a is removed, and the first c that appears after the removed b is
; removed.
; ------------------------------------------------------------------------------

(define removesubsequence
  (lambda (subseq lis)
    (define (remove-helper subseq lis)
      (cond
        ((or (null? subseq) (null? lis)) lis)
        ((equal? (car subseq) (car lis))
         (remove-helper (cdr subseq) (cdr lis)))
        (else
         (cons (car lis) (remove-helper subseq (cdr lis))))
      )
    )
    (remove-helper subseq lis)
  ) ; end lambda
) ; end define

; ------------------------------------------------------------------------------
; 5. reverse* takes a nested list and reverses the contents of the list and all
; nested lists.
; ------------------------------------------------------------------------------

(define reverse*
  (lambda (lis)
    (cond
      ((null? lis) '())
      (else
        (append
          (reverse* (cdr lis))
          (list
            (if (list? (car lis))
              (reverse* (car lis))
              (car lis)
            ) ; end if
          ) ; end list
        ) ; end append
      ) ; end else
    ) ; end cond
  ) ; end lambda
) ; end define

; ------------------------------------------------------------------------------
; 6. first* takes a list of lists and returns the first (left most) atom that
; appears in the list, regardless of how nested it is.
; ------------------------------------------------------------------------------

(define first*
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((not (list? (car lis))) (car lis))
      (else (first* (car lis)))
    ) ; end cond
  ) ; end lambda
) ; end define

; ------------------------------------------------------------------------------
; 7. last* takes a list of lists and returns the last (right most) atom that
; appears in the list, regardless of how nested it is. Give a simple solution
; that does not reverse the list.
; ------------------------------------------------------------------------------

(define last*
  (lambda (lis)
    (cond

      ((null? lis) '())
      ((not (list? (car lis)))
        (if (null? (cdr lis))
          (car lis)
          (last* (cdr lis))
        ) ; end if
      ) ; end not condition

      (else
        (if (null? (last* (cdr lis)))
          (last* (car lis))
          (last* (cdr lis))
        ) ; end if
      ) ; end else

    ) ; end cond
  ) ; end lambda
) ; end define

; ------------------------------------------------------------------------------
; 8. numorder*? takes a possibly nested list of numbers, and returns #t if the
; values of the entries in the list and all sublists are in non-decreasing
; order. The value of a number is the number. The value of a list is the sum of
; the values in that list.
; > (numorder*? '((() ()) 1 (2) (2 3 (-1 4) 5) (((4) 5) 10) 20))
; (0 1 2 13 39)
; #t
; > (numorder*? '((() ()) 1 (2) (2 3 (4 -1) 5) (((4) 5) 10) 20))
; (0 1 2 13 39) - I don't see how this is false.
; #f
; > (numorder*? '(1 (2) (2 3 (-1 4)) 5))
; (1 2 8 5)
; #f
; ------------------------------------------------------------------------------

(define numorder*?
  (lambda (lis)
    
    (define (sum-list lis)
      (if (null? lis)
        0
        [+ (value (car lis)) (sum-list (cdr lis))]
      )
    )

    (define (value item)
      (if (list? item)
        [sum-list item]
        item
      )
    )

    (define (transform-list lis)
      (if (null? lis)
        '()
        [cons (value (car lis)) (transform-list (cdr lis))]
      )
    )

    [inorder? (transform-list lis)]

  ) ; end lambda
) ; end define


;(define (contains-sublist? lis)
;  (cond
;    [(null? lis) #f]
;    [(list? (car lis)) #t]
;    [else
;      (contains-sublist? (cdr lis))
;    ]
;  ) ; end cond
;) ; end define

; Test cases
;(contains-sublist? '(a b c)) ; => #f
;(contains-sublist? '(a (b) c)) ; => #t
;(contains-sublist? '(a b c (d e))) ; => #t
;(contains-sublist? '()) ; => #f

;(define numorder*?
;  (lambda (lis)
;    (cond
;      ((contains-sublist? lis) (numorder*? (cdr lis)))
;      ((cons (sum-list lis) lis))
;      ((not (inorder? lis)) #f)
;      ;(else #t)
;    )
;  )
;)


; ------------------------------------------------------------------------------
; 9. vectormult takes a row vector (a list of numbers) and matrix (a list of
; lists of numbers) and multiplies the vector times the matrix. The result is a
; vector where the ith element of the result is the dotproduct of the input
; vector and the ith column of the matrix. You can assume that the length of the
; vector matches the number of rows of the matrix.
; ------------------------------------------------------------------------------

(define vectormult
  (lambda(vec mat)

    (define (transpose m)
      (if (null? (car m))
        '()
        (cons (build-row m) (transpose (remove-heads m)))
      )
    )

    (define (build-row m)
      (if (null? m)
        '()
        (cons (car (car m)) (build-row (cdr m)))
      )
    )

    (define (remove-heads m)
      (if (null? m)
        '()
        (cons (cdr (car m)) (remove-heads (cdr m)))
      )
    )

    (define (mult-helper v m)
      (if (null? m)
        '()
        (cons (dotproduct v (car m)) (mult-helper v (cdr m)))
      )
    )

  (mult-helper vec (transpose mat))

  ) ; end lambda
) ; end define


; ------------------------------------------------------------------------------
; 10. matrixmultiply takes two matrices (a list of lists of numbers) and
; multiplies them. You can assume the number of columns of the first matrix is
; equal to the number of rows of the second matrix. in the same sublist
; ------------------------------------------------------------------------------

(define (transpose mat)
  (define (first-elements mat)
    (if (null? mat)
      '()
      (cons (car (car mat)) (first-elements (cdr mat)))
    )
  )
  
  (define (remove-first-elements mat)
    (if (null? mat)
      '()
      (cons (cdr (car mat)) (remove-first-elements (cdr mat)))
    )
  )

  (if (null? (car mat))
    '()
    (cons (first-elements mat) (transpose (remove-first-elements mat)))
  )
)

(define matrixmultiply
  (lambda (mat1 mat2)
    (define transposed-mat2 (transpose mat2))
    (define (multiply-row row mat)
      (if (null? mat)
        '()
        (cons (dotproduct row (car mat)) (multiply-row row (cdr mat)))
      )
    )

  (define (multiply-matrix mat)
    (if (null? mat)
      '()
      (cons
       (multiply-row (car mat) transposed-mat2)
       (multiply-matrix (cdr mat))
      )
    )
  )

  (multiply-matrix mat1)
  ) ; end lambda
) ; end define
