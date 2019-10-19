(defun _print-lines (file)
    (if file
        (or
            (loop for line = (read-line file nil)
                while line do (format t "~a~%" line)
            )
            (close file)
        )
        (format t "~a~%" "[!] Error while opening file")
    )
)
(defun print-lines (path)
    (_print-lines (open path :if-does-not-exist nil))
)

(print-lines "error")
(print-lines "sample.txt")
