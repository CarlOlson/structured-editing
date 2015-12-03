
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

(defun se-line-start-p (&optional POINT)
  "Returns true if POINT is the first non-whitespace character on
the current line. Uses current point if POINT is nil."
  (unless POINT (setq POINT (point)))
  (save-excursion
    (back-to-indentation)
    (= (point) POINT)))

(defun se-same-line-p (A B)
  "Returns true if points A and B are on the same line."
  (= (line-number-at-pos A) (line-number-at-pos B)))

(defun se-each-line (FUNCTION &optional BOUND)
  "Goes to the start of each line and evaluates FUNCTION. Order
starts at bottom and goes up to current line. If BOUND is non-nil
evalutaions will start at BOUND instead of `point-max'."
  (when (or (null BOUND) (> BOUND (point)))
    (save-excursion
      (beginning-of-line)
      (let ((end-point (point))
	    (line-move-visual nil))
	(goto-char (or BOUND (point-max)))
	(while (>= (point) end-point)
	  (beginning-of-line)
	  (save-excursion
	    (funcall FUNCTION))
	  (forward-line -1))))))

(provide 'se-helpers)
