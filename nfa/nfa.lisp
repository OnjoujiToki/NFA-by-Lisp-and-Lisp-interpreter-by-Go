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
    
(defun not-equal (a b) 
    (if (equal a b) nil
        T
    )
    
)
(defun reachable (transition start final input)
     ;;(setf cur start)
     ;;(funcall transition start (car input))
    
     (cond ((equal (funcall transition start (car input)) ') nil) (t T))    
     ;;(if (null input) (if (eql start final) T nil)
      ;; (reachable transition (+ start 1) final (cdr input)))
   
)

;;(print (fooTransitions 0 'B))
;;(print (fooTransitions 0 'A))
(print (reachable 'expTransitions 0 3 '(3 B A)))
;;(print (contains '(1 2 3) 1))
;;(print (not-equal '(nil) ) )
