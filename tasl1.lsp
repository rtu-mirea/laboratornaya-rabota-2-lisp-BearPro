(defun sample ()
    `(5, 6, 1, 3, 4, 12, 54, 19, 76, 2, 3, 1, 0)
)

(defun _insert_at (head tail at value current)
    (if (= at current)
        (append head (cons value tail))
        (_insert_at 
            (append head (list (car tail)))
            (cdr tail)
            at 
            value 
            (+ 1 current )
        )
    )
)

(defun _remove_at (head tail at current)
    (if (= at current)
        (append head (cdr tail))
        (_remove_at 
            (append head (list (car tail)))
            (cdr tail)
            at
            (+ 1 current )
        )
    )
)

(defun _index_of (value lst index)
    (if (= (car lst) value)
        index
        (_index_of value (cdr lst) (+ 1 index))
    )
)

(defun index_of (value lst)
    (_index_of value lst 0)
)

(defun insert_at (lst at value)
    (_insert_at `() lst at value 0)
)

(defun remove_at (index lst)
    (_remove_at `() lst index 0)
)


(insert_at (sample) 4 999999)
(index_of 12 (sample))
(remove_at 1 (sample))