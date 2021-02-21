(defun fooTransitions (state input)
    (cond
    ((and (eq state 0) (eq input 'A ))  (list 1 2))
    ((and (eq state 0) (eq input 'B ))  (list 2))
    ((and (eq state 1) (eq input 'B ))  (list 3))
    ((and (eq state 2) (eq input 'c ))  (list 3))
    (t (list nil))
    )
)

(defun expTransitions (state input)
	;;  * 0 -a-> 1
	;;  * 0 -a-> 2
	;;  * 0 -b-> 2
	;;  * 1 -b->0
    (cond
    ((and (eq state 0) (eq input 'A ))  (list 1 2))
    ((and (eq state 0) (eq input 'B ))  (list 2))
    ((and (eq state 1) (eq input 'B ))  (list 0))
    (t (list nil))
    )
)

(defun contains (lst target)
    (cond ((eql (length lst) 0) nil) ((eql (car lst) target) T) (t (contains (cdr lst) target))) )     
    

(defun reachable (transition start final input)
     (setf cur start)
     (print (funcall transition cur (car input)))
     (if (null input) (if (eql cur 0) 4 )
       (reachable transition start cur (cdr input)))
   
)

;;(print (fooTransitions 0 'B))
;;(print (fooTransitions 0 'A))
(print (reachable 'expTransitions 0 2 '(A B A)))
(print (contains '(1 2 3) 1))
