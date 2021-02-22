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
    (if (< (length matrix) 1) matrix
        (apply 'mapcar 'list matrix)
        )
    
)

; AreNeighbors returns true iff a and b are neighbors in the 2D
; matrix mat.
(defun are-neighbors-vertical (matrix a b)
    (cond ((are-adjacent (car matrix) a b) T)
          ((< (length matrix) 2) nil)
          ( t  (are-neighbors-vertical (cdr matrix) a b))  )
        
    )
(defun are-neighbors (matrix a b)
    (if (< (length matrix) 2) (are-neighbors-vertical matrix a b)
         (cond ((are-neighbors-vertical matrix a b) T)
          ((are-neighbors-vertical (transpose matrix) a b) T)
          ( t  NIL)  )
        
        ) 
    )

