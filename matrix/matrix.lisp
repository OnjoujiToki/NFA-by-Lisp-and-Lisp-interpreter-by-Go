(defun  are-adjacent (lst a b)
    ;;(format t "~A" x)
    (if (< (length lst) 2 )
        nil
    (if (or (and (eql (car lst) b) (eql (cadr lst) a)) (and (eql (car lst) a) (eql (cadr lst) b)))
        T
        (are-adjacent (cdr lst) a b)))
 )

; Transpose returns the transpose of the 2D matrix mat.
(defun transpose (matrix)
    ;; TODO: Incomplete function
    (apply 'mapcar 'list matrix)
)

; AreNeighbors returns true iff a and b are neighbors in the 2D
; matrix mat.
(defun are-neighbors-vertical (matrix a b)
    (cond ((are-adjacent (car matrix) a b) T)
          ((eql (length matrix) 1) nil)
          ( t  (are-neighbors-vertical (cdr matrix) a b))  )
        
    )
(defun are-neighbors (matrix a b)
    (cond ((are-neighbors-vertical matrix a b) T)
          ((are-neighbors-vertical (transpose matrix) a b) T)
          ( t  NIL)  )
        
   
    
    
    )
;;(print (are-adjacent '(1 2 3) 1 3))
(print (are-neighbors '( (1 2 3) (4 5 6) ) 1 2))
