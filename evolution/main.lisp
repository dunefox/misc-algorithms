(defparameter *ops* '(+ - * ))
(defparameter *max-depth* 5)
(defparameter *max-nodes* 5)
(defparameter *max-range* 5)
(defparameter *numbers* (loop :for n :below *max-range* :collect (+ 1 n)))
(defparameter *elts* (append *ops* *numbers*))
(defparameter *mutations* '(:num :num :tree :tree :tree :del :ins :ins))

(defun make-tree (symbol depth)
  (cond ((<= depth 0) (elt *numbers* (random *max-range*)))
        ((not (member symbol *ops*)) symbol)
        (t (let* ((slots (+ 2 (random *max-nodes*)))
                  (tree-level (loop :repeat slots :collect (elt *elts* (random (length *elts*))))))
             (cons symbol (mapcar (lambda (x) (make-tree x (- depth 1))) tree-level))))))

(defun calc-depth (tree)
  (progn
    (defun go_ (form &optional (depth 1))
      (let ((cleaned (remove-if-not #'listp form)))
        (cond ((eq cleaned '()) depth)
              (t (mapcar (lambda (x) (go_ x (1+ depth))) cleaned)))))
    (reduce #'max (flatten (go_ tree)))))

(defun mutate-tree (tree)
  (progn
    (defparameter *d* (random (calc-depth tree)))
    (defun go_ (form depth)
      (let* ((action (elt *mutations* (random (length *mutations*))))
             (rnd-ind (1+ (random (1- (length form)))))
             (rnd-form (elt form rnd-ind)))
        (progn
          (print (format nil "form: ~A" form))
          (print (format nil "depth: ~A" depth))
          (print (format nil "ind: ~A" rnd-ind))
          (print (format nil "rnd-form: ~A" rnd-form))
          (print (format nil "action: ~A" action))
          (cond
            ((and (= 1 (length form))
                  (member (car form) *ops*))
             (return-from go_ :dead))
            ((and (> depth 0)
                  (>= (random 1.0) 0.3)
                  (listp rnd-form))
             (go_ rnd-form (- depth 1)))
            ((eq action :tree) (setf (elt form rnd-ind) (make-tree (elt *ops* (random (length *ops*))) 1)))
            ((eq action :num)  (setf (elt form rnd-ind) (random *max-range*)))
            ((eq action :del)  (progn
                                 (setf (elt form rnd-ind) nil)
                                 (delete nil form)
                                 form))
            ((eq action :ins)  (nconc (make-tree (elt *ops* (random (length *ops*))) (1- (random *max-depth*))) (list form)))
            (t (print "else"))))))
    (print tree)
    (go_ tree *d*)))

;; TODO: mutate, select, fitness, crossover
;; mutate: select random node, swap children for (make-tree rand-symb (- *max-depth* calc-depth))
;;      || delete subtrees, replace with number
;;      || select random number, replace with new make-tree
;; TODO: Random symbol for root of tree
;; TODO: refactor go_ into generic walk-tree?

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))
