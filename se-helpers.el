
;; (eval-when-compile (require 'cl))

(defmacro curry (f &rest args)
  `(lambda (&rest more-args)
     (apply ',f ,@args more-args)))

(defun between (x a b)
  "Checks for `x' in interval [a, b]."
  (and
   (>= x a)
   (<= x b)))

(defun map-1 (fn list)
  "Maps elements of `list' onto `fn', return first non-nil
transformed element."
  (loop while list
	thereis (funcall fn (pop list))))

(defun cons-t (first rest)
  (if first
      (cons first rest)
    rest))

(defun split-list (delimiter alist)
  (split-list-if (lambda (x) (equal x delimiter)) alist))

(defun split-list-1 (delimiter alist)
  (split-list-if-1 (lambda (x) (equal x delimiter)) alist))

(defun split-list-if (pred alist)
  (when alist
    (let ((split (split-list-if-1 pred alist)))
      (cons (first split)
	    (split-list-if pred (rest split))))))

(defun split-list-if-1 (pred alist)
  (loop for (x . xs) on alist
	until (funcall pred x)
	collecting x into partial
	finally (return (cons partial xs))))
