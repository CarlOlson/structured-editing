
;; (eval-when-compile (require 'cl))

(defmacro se-curry (f &rest args)
  "Returns curried function. `f' should be a function symbol."
  `(lambda (&rest more-args)
     (apply ,f ,@args more-args)))

(defun se-between (x a b)
  "Checks for `x' in interval [a, b]."
  (and
   (>= x a)
   (<= x b)))

(defun se-map-1 (fn list)
  "Maps elements of `list' onto `fn', return first non-nil
transformed element."
  (loop while list
	thereis (funcall fn (pop list))))

(defun se-cons-t (first rest)
  "Conses `first' to `rest' if `first' is non-nil, otherwise
returns `rest'."
  (if first
      (cons first rest)
    rest))

;; (defun se-split-list (delimiter alist)
;;   (split-list-if (lambda (x) (equal x delimiter)) alist))

;; (defun se-split-list-1 (delimiter alist)
;;   (split-list-if-1 (lambda (x) (equal x delimiter)) alist))

;; (defun se-split-list-if (pred alist)
;;   (when alist
;;     (let ((split (split-list-if-1 pred alist)))
;;       (cons (first split)
;; 	    (split-list-if pred (rest split))))))

;; (defun se-split-list-if-1 (pred alist)
;;   (loop for (x . xs) on alist
;; 	until (funcall pred x)
;; 	collecting x into partial
;; 	finally (return (cons partial xs))))
