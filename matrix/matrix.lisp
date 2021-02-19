; A list is a 1-D array of numbers.
; A matrix is a 2-D array of numbers, stored in row-major order.

; If needed, you may define helper functions here.

; AreAdjacent returns true iff a and b are adjacent in lst.
(defun  are-adjacent (lst a b)
    (let ((x 0)))
    (setf x (length lst))
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
    (apply #'mapcar #'list matrix)
)

; AreNeighbors returns true iff a and b are neighbors in the 2D
; matrix mat.
(defun are-neighbors (matrix a b)
    ;; TODO: Incomplete function
    (list 'incomplete)
)

