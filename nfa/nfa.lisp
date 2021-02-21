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

(defun langTransitions (state input)
	;;  * 0 -a-> 0
	;;  * 0 -b-> 1
	;;  * 1 -a-> 1
	;;  * 1 -b-> 0
  (cond
    ((and (eq state 0) (eq input 'A ))  (list 0))
    ((and (eq state 0) (eq input 'B ))  (list 1))
    ((and (eq state 1) (eq input 'A ))  (list 1))
    ((and (eq state 1) (eq input 'B ))  (list 0))
    (t (list nil))
    )

)

(defun contains (lst target)
    (cond ((eql (length lst) 0) nil) ((eql (car lst) target) T) (t (contains (cdr lst) target))) )     
    
(defun not-empty (a) 
    (if (equal a '(nil)) nil
        T
    )
    
)

(defun is-empty (a)  ;; run out of choices
    (if (equal a '(nil)) T
        nil
    )
    
)

(defun reachable-bc (transitition lst)
    nil)
(defun reachable (transition start final input)
     ;;(setf cur start)
     ;;(funcall transition start (car input))
       
     (cond 
         ((null input) (if (eql start final) T ))
         ( (not-empty (funcall transition start (car input))) (reachable transition (car (funcall transition start (car input))) final (cdr input) )) 
         (t nil)
      )    
      
     ;;(if (null input) (if (eql start final) T nil)
      ;; (reachable transition (+ start 1) final (cdr input)))
   
)

;;;;;;;;;;;
 ;;(assert-equal NIL (reachable 'fooTransitions 0 3 '(A B C)))
  ;;  (assert-equal T (reachable 'fooTransitions 0 3 '(A B)))
   ;; (assert-equal NIL (reachable 'fooTransitions 0 3 '(A A A)))
  ;;  (assert-equal T (reachable 'fooTransitions 0 3 '(A C)))
;;;;;;;;;;;;;;;
(print (reachable 'langTransitions 0 0 NIL)) ;; T
(print (reachable 'langTransitions 0 1 '(B))) ;; T
(print (reachable 'langTransitions 0 0 '( A A A))) ;;T
(print (reachable 'langTransitions 0 1 '( A B B A))) ;;NIl
(print (reachable 'langTransitions 0 1 '( B A B A B))) ;;T

;;;;;;
;;(print (contains '(1 2 3) 1))
;;(print (not-equal '(nil) ) )
