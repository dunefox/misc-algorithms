(defparameter *ops* '(+ - * /))
(defparameter *max-depth* 5)
(defparameter *max-nodes* 5)

(defun make-tree (symbol depth)
  (cond ((= depth 0) 1)
        ((not (member symbol *ops*)) symbol)
        (t (let* ((alpha (+ 1 (random *max-nodes*)))
                  (elts (concatenate 'list *ops* (loop :for n :below 10 collect (+ 1 n))))
                  (tree-level (loop for i from 1 to alpha collect (nth (+ 1 (random (length elts))) elts))))
             (cons symbol (mapcar (lambda (x) (make-tree x (- 1 depth))) tree-level))))))

