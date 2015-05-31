
(eval-when-compile (require 'cl))

(defmacro curry (f &rest args)
  `(lambda (&rest more-args)
     (apply ',f ,@args more-args)))

(defmacro pop-while (lst conditional)
  "Removes elements from `lst' until `conditional' is false. The
removed elements are returned. This will modify the state of
`lst'."
  (assert (symbolp lst))
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
			 (pop-while tokens (is-token-child head parent))))
       (sorted-tokens-to-tree tokens))))))

(eval-when-compile
  (assert (token< (new-token "less" 1  20)
		  (new-token "more" 21 40)))

  (assert (token< 
	   (new-token "first"  1 40)
	   (new-token "second" 1 20)))

  (assert (is-token-child (new-token "child" 1 2)
			  (new-token "main" 1 10)))

  (setq pos-test-data
	(mapcar
	 (curry apply 'new-token)
	 '(("L1" 1  100)
	   ("L2" 1   20)
	   ("L2" 21  50)
	   ("L2" 51 100)
	   ("L3" 1   10)
	   ("L3" 30  40)
	   ("L3" 41  50))))

  (setq neg-test-data
	(cons (new-token "L4" 19 21) pos-test-data))

  (assert
   (create-parse-tree pos-test-data))
  
  (assert
   (null
    (create-parse-tree neg-test-data)))
  )
