
(defstruct
    (span
     (:constructor new-span (type start end))
     (:constructor nil-span ()))
  type start end)

(defstruct
    (node
     (:constructor new-node (parent children))
     (:constructor nil-node ()))
     parent children)

(defun span< (a b)
  "Checks if span `a' should come before `b'. A span spanning 1
to 100 would be before 1 to 20 because it encapsulates it."
  (or
   (< (span-start a)
      (span-start b))
   (and
    (= (span-start a)
       (span-start b))
    (> (span-end a)
       (span-end b)))))

(defun is-span-child (child parent)
  (and
   (>= (span-start child)
       (span-start parent))
   (<= (span-end child)
       (span-end parent))))


(lexical-let (spans parents)
  (defun create-parse-tree (lst)
    "Forms a tree from span information. This will change the
state of spans to be sorted. Returns nil if data is ill
formatted."
    ;; `copy-list' could be used; however, it isn't expected a user will
    ;; reuse a span list (or care if it becomes sorted).
    (setq spans lst)
    (setq parents lst)
    (let ((len (length lst)))
      (sort spans 'span<)
      (when (= len (length lst)) (sorted-spans-to-tree))))

  (defun sorted-spans-to-tree ()
    (cond
     ((null spans) nil)
     ((or (null parents)
	  (is-span-child (first spans) (first parents)))
      (push (pop spans) parents)
      (cons
       (new-node (first parents) (sorted-spans-to-tree))
       (sorted-spans-to-tree)))
     (:else
      (pop parents)
      nil)))
  )
