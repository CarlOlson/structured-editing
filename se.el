
(defstruct
    (token
     (:constructor new-token (type start end))
     (:constructor nil-token ()))
  type start end)

(defstruct
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


(lexical-let (tokens parents)
  (defun create-parse-tree (lst)
    "Forms a tree from token information. This will change the
state of tokens to be sorted. Returns nil if data is ill
formatted."
    ;; `copy-list' could be used; however, it isn't expected a user will
    ;; reuse a token list (or care if it becomes sorted).
    (setq tokens lst)
    (setq parents lst)
    (let ((len (length lst)))
      (sort tokens 'token<)
      (when (= len (length lst)) (sorted-tokens-to-tree))))

  (defun sorted-tokens-to-tree ()
    (cond
     ((null tokens) nil)
     ((or (null parents)
	  (is-token-child (first tokens) (first parents)))
      (push (pop tokens) parents)
      (cons
       (new-node (first parents) (sorted-tokens-to-tree))
       (sorted-tokens-to-tree)))
     (:else
      (pop parents)
      nil)))
  )
