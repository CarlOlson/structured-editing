
;; (eval-when-compile (require 'cl))

(defmacro curry (f &rest args)
  `(lambda (&rest more-args)
     (apply ',f ,@args more-args)))

(defun between (x a b)
  "Checks for `x' in interval [a, b]."
  (and
   (>= x a)
   (<= x b)))
