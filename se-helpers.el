
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

(defmacro se-swap-minmax (MIN MAX)
  "Swaps MIN and MAX if MIN is greater than MAX."
  (let ((min (gensym)))
    `(when (> ,MIN ,MAX)
       (let ((,min ,MIN))
	 (setq ,MIN ,MAX
	       ,MAX ,min)))))

(defun se-mode-line-start-p (&optional POINT)
  "Returns true if POINT is the first non-whitespace character on
the current line. Uses current point if POINT is nil."
  (unless POINT (setq POINT (point)))
  (save-excursion
    (goto-char POINT)
    (beginning-of-line)
    (cond
     ((= POINT (point)) t)
     ((> POINT (point))
      (re-search-forward "[\s\t]+" POINT t)
      (and
       (not (memq (char-after) (list ?\s ?\t)))
       (= POINT (point))))
     (t nil))))

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
	  (previous-line))))))

(provide 'se-helpers)
