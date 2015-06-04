
(eval-when-compile (require 'cl))

(defmacro curry (f &rest args)
  `(lambda (&rest more-args)
     (apply ',f ,@args more-args)))

(defmacro list-help (lst &rest body)
  (assert (or (symbolp lst)
	      (listp lst)))
  `(let ((head (car ,lst))
	 (rest (cdr ,lst)))
     ,@body))

(defun parse-varlist (varlist)
  (list-help varlist
	     (cond
	      ((null varlist) nil)
	      ((symbolp head)
	       (cons (list head nil)
		     (parse-varlist rest)))
	      ((listp head)
	       (cons head
		     (parse-varlist rest)))
	      (:else
	       (error (format "Cannot parse `%s' with list-help" head))))))

(defun zip (a b)
  (cond
   ((or (null a)
	(null b)) nil)
   (:else
    (cons (list (car a) (car b))
	  (zip (cdr a) (cdr b))))))

(defmacro let-gensym (varlist &rest body)
  (setq varlist (parse-varlist varlist))
  (setq gensyms (mapcar (lambda (a)
			  `(,(car a) ,(gensym)))
			varlist))
  (setq varlist (zip (mapcar 'second gensyms)
		     (mapcar 'second varlist)))
  `(symbol-macrolet ,gensyms
     (let ,varlist
       ,@body)))

(defmacro pop-from (lst when conditional)
  "Removes elements from `lst' until a condition is met. `when'
may be equal to `while' or `until'. For `while' `conditional'
must be false for termination. For `until' `conditional' must be
true for termination. The removed elements are returned. This
will modify the state of `lst'."
  (assert (symbolp lst))
  (assert (consp conditional))
  (assert (member when '(while until)))
  (if (eq when 'until)
      (setq conditional (list 'not conditional)))
  `(let-gensym ((rtn (list nil))
		(head (car ,lst)))
	       (while (and ,lst ,conditional)
		 (nconc rtn (list (pop ,lst)))
		 (setq head (car ,lst)))
	       (cdr rtn)))
