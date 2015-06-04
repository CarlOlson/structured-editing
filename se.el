
(eval-when-compile (require 'cl))

(defmacro curry (f &rest args)
  `(lambda (&rest more-args)
     (apply ',f ,@args more-args)))

(defmacro pop-from (lst when conditional)
  "Removes elements from `lst' until `conditional' is false. The
removed elements are returned. This will modify the state of
`lst'."
  (assert (symbolp lst))
  (assert (member when '(while until)))
  (if (eq when 'until)
      (setq conditional (list 'not conditional)))
  (let ((rtn (gensym)))
    `(let ((,rtn (list nil))
	   (head (car ,lst)))
       (while (and ,lst ,conditional)
	 (nconc ,rtn (list (pop ,lst)))
	 (setq head (car ,lst)))
       (cdr ,rtn))))

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

