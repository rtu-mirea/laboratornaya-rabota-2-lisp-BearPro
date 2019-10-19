(defconstant sample (list 1 0 0 0 1 1 1 0 0 0))

(defun redf (all i)
    (if (= (nth 1 i) (nth 1 (car all)))
        (append (list (list (+ 1 (nth 0 (car all))) (nth 1 i))) (cdr all))
        (append (list i) all)
    )
)

(defun compress (lst)
    (map 'list #'unmapf
    (cdr
    (reverse
    (reduce
        #'redf
        (map 'list #'mapf lst)
        :initial-value (list(list 1 -1))
    ))))
)
(defun mapf (i) (list 1 i))
(defun unmapf (i) 
    (if (= (nth 0 i) 1)
        (nth 1 i)
        i
    )
)
(compress sample) ; => (1 (3 0) (3 1) (3 0))