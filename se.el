
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

(cl-defstruct
    (token
     (:constructor new-token (type start end))
     (:constructor nil-token ()))
  type start end)

(cl-defstruct
    (node
     (:constructor new-node (parent children))
     (:constructor nil-node ()))
     parent children)

(defun token< (a b)
  "Checks if token `a' should come before `b'. A token spanning 1
to 100 would be before 1 to 20 because it encapsulates it."
  (or
   (< (token-start a)
      (token-start b))
   (and
    (= (token-start a)
       (token-start b))
    (> (token-end a)
       (token-end b)))))

(defun is-token-child (child parent)
  (and
   (>= (token-start child)
       (token-start parent))
   (<= (token-end child)
       (token-end parent))))

(defun create-parse-tree (tokens)
  "Forms a tree from token information. This will change the
state of tokens to be sorted. Returns nil if data is ill
formatted."
  ;; `copy-list' could be used; however, it isn't expected a user will
  ;; reuse a token list.
  (let ((len (length tokens)))
    (sort tokens 'token<)
    (if (= (length tokens) len)
	(sorted-tokens-to-tree tokens))))

(defun sorted-tokens-to-tree (tokens)
  (cond
   ((null tokens) nil)
   (:else
    (let ((parent (pop tokens)))
      (cons
       (new-node parent (sorted-tokens-to-tree
			 (pop-from tokens while (is-token-child head parent))))
       (sorted-tokens-to-tree tokens))))))

