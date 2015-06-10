
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

(defun create-parse-tree (lst)
  "Forms a tree from span information. This will change the
state of spans to be sorted. Returns nil if data is ill
formatted."
  ;; `copy-list' could be used; however, it isn't expected a user will
  ;; reuse a span list (or care if it becomes sorted).
  (let ((spans lst)
	(parents nil)
	(len (length lst)))
    (sort spans 'span<)
    (when (= len (length lst)) (sorted-spans-to-tree))))

(byte-compile #'create-parse-tree)

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

(let ((byte-compile-warnings '(not free-vars)))
  (byte-compile #'sorted-spans-to-tree))

(defun is-point-in-span (point span)
  "Checks for point inside given span."
  (between point (span-start span) (span-end span)))

(defun find-min-span (point tree)
  "Finds the deepest span in `tree' that contains `point'."
  (typecase tree
    (node
     (when (is-point-in-span point (node-parent tree))
       (or (find-min-span point (node-children tree))
	   (node-parent tree))))
    (list
     (map-first (curry find-min-span point) tree))))

(byte-compile #'find-min-span)
