
(eval-when-compile (require 'cl))

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

(provide 'se-helpers)
