
(defstruct
    (se-span
     (:constructor se-new-span (type start end)))
  type start end)

(defstruct
    (se-node
     (:constructor se-new-node (parent children)))
     parent children)

(defun se-flatten (tree)
  "Flattens a tree of nodes, spans, and lists into a single list
of spans. This keeps the order of elements but is inefficient."
  (typecase tree
    (null)
    (se-span
     (list tree))
    (se-node
     (cons (se-node-parent tree)
	   (se-flatten (se-node-children tree))))
    (sequence
     (loop for node in tree
	   collecting (se-flatten node) into nodes
	   finally (return (apply #'append nodes))))))

(defun se-as-spans (term)
  "se-* methods favor returning nodes instead of
spans. `se-as-spans' will convert to a list of spans
instead. This will not flatten `term'."
  (typecase term
    (se-span term)
    (se-node
     (se-as-spans (se-node-parent term)))
    (sequence
     (mapcar #'se-as-spans term))))

(defun se-first-span (term)
  (typecase term
    (se-span term)
    (se-node (se-first-span (se-node-parent term)))
    (cons (se-first-span (first term)))	;; make work with sequences
    (t (signal 'wrong-type-argument '(term)))))

(defun se-last-span (term)
  (typecase term
    (se-span term)
    (se-node
     (if (se-node-children term)
	 (se-last-span (se-node-children term))
       (se-last-span (se-node-parent term))))
    (cons ;; make work with sequences
     (se-last-span (first (last term))))
    (t (signal 'wrong-type-argument '(term)))))

(defun se-term-start (term)
  (se-span-start (se-first-span term)))

(defun se-term-end (term)
  (se-span-end (se-last-span term)))

(defun se-term-length (term)
  (1+ (- (se-term-end term)
	 (se-term-start term))))

(defun se-point-in-term-p (point term)
  "Checks if `point' is contained within the spans of `term'."
  (between point (se-term-start term) (se-term-end term)))

(defun se-term-equal-p (term1 term2)
  "Compares the start and end points of `term1' and `term2'. This
should be what equality generally means for terms."
  (and
   (equal (se-term-start term1) (se-term-start term2))
   (equal (se-term-end term1) (se-term-end term2))))

(defun se-term-before-p (a b)
  "Checks if span `a' should come before `b'. A span spanning 1
to 100 would be before 1 to 20 because it encapsulates it."
  (let ((a-start (se-term-start a))
	(b-start (se-term-start b)))
  (or
   (< a-start
      b-start)
   (and
    (= a-start
       b-start)
    (> (se-term-end a)
       (se-term-end b))))))

(defun se-term-child-p (child parent)
  "Checks if `child' should be encapsulated by `parent'. The
bounds of `child' should be inside the bounds of `parent'."
  (and
   (>= (se-term-start child)
       (se-term-start parent))
   (<= (se-term-end child)
       (se-term-end parent))))

(defun se-create-parse-tree (lst)
  "Forms a tree from span information. This will change the
state of spans to be sorted. Returns nil if data is ill
formatted."
  ;; `copy-list' could be used; however, it isn't expected a user will
  ;; reuse a span list (or care if it becomes sorted).
  (let ((spans lst)
	(parents nil)
	(len (length lst)))
    (sort spans #'se-term-before-p)
    (when (= len (length lst)) (se-sorted-spans-to-tree))))

(defun se-sorted-spans-to-tree ()
  (cond
   ((null spans) nil)
   ((or (null parents)
	(se-term-child-p (first spans) (first parents)))
    (push (pop spans) parents)
    (cons
     (se-new-node (first parents) (se-sorted-spans-to-tree))
     (se-sorted-spans-to-tree)))
   (:else
    (pop parents)
    nil)))

(defun se-find-point (point tree)
  "Finds the deepest node in `tree' that contains `point'."
  (typecase tree
    (se-node
     (when (se-point-in-term-p point (se-node-parent tree))
       (if (se-node-children tree)
	   (se-find-point point (se-node-children tree))
	 tree)))
    (sequence
     (map-1 (curry se-find-point point) tree))))

(defun se-find-point-path (point tree)
  "Finds a series of nodes in `tree' containing `point'. Returns
a list containing nodes with the former elements as parents of
the latter."
  (typecase tree
    (se-node
     (when (se-point-in-term-p point (se-node-parent tree))
       (cons tree
	     (se-find-point-path point (se-node-children tree)))))
    (sequence
     (map-1 (curry se-find-point-path point) tree))))

(defun se-find-span (span tree)
  "Finds a node in `tree' with parent span equal to
`span'. Returns `nil' if no node matches."
  (typecase tree
    (se-node
     (if (equal span (se-node-parent tree))
	 tree
       (map-1 (curry se-find-span span) (se-node-children tree))))
    (sequence
     (map-1 (curry se-find-span span) tree))))

(defun se-find-span-path (span tree)
  "Finds a series of nodes in `tree' containing each other ending
with a node with parent equal to `span'. Returns a list
containing nodes with the former elements as parents of the
latter. Returns `nil' if no node matches."
  (typecase tree
    (se-node
     (cond
      ((se-term-equal-p span (se-node-parent tree))
       (list tree))
      ((se-term-child-p span (se-node-parent tree))
       (let ((temp (se-find-span-path span (se-node-children tree))))
	 (when temp
	   (cons tree temp))))))
    (sequence
     (map-1 (curry se-find-span-path span) tree))))

(defun se-find-after (term tree)
  "Collects all nodes in `tree' after reaching `term'. The node
of `term' isn't kept, nor its children."
  (typecase tree
    (se-node
     (if (se-term-equal-p term tree)
	 nil
       (se-find-after term (se-node-children tree))))
    (sequence
     (loop for (first second . nodes) on tree
	   when (null second) do (return (se-find-after term first))
	   when (se-term-before-p term second)
	   do (return (append (se-find-after term first)
			      (cons second nodes)))))))

;; currently trying to determine expected functionality
(defun se-find-before (term tree)
  "Collects all nodes in `tree' until reaching `term'. The node
of `term' isn't kept."
  (error "Method `se-find-before' not implemented.")
  (typecase tree
    (se-node)
    (sequence
     (loop for (first second . nodes) in tree
	   collecting first into before
	   while (and second (se-term-before-p second term))
	   finally (return (append before (se-find-before first)))))))

(defun spans-find-between (span1 span2 tree)
  (let (start-span-found end-span-found)
    (if (span< span1 span2)
	(spans-before-span span2 (spans-after-span span1 tree)))
    (spans-before-span span1 (spans-after-span span2 tree))))

(defun se-span-add-offset (offset span)
  (incf (se-span-start span) offset)
  (incf (se-span-end span) offset))

(defun se-term-add-offset (offset node)
  (typecase node
    (se-span
     (se-span-add-offset node offset))
    (se-node
     (se-span-add-offset (se-node-parent node) offset)
     (se-term-add-offset (se-node-children node) offset))
    (cons
     (se-term-add-offset (first node) offset)
     (se-term-add-offset (rest node) offset))))

(defun se-term-expand-to (node point)
  (error "Method `se-node-expand-to' not functional.")
  (typecase node
    (se-span
     (when (< (se-span-end node) point)
       (setf (se-span-end node) point)
       node))
    (se-node
     (cons-t
      (se-term-expand-to (se-node-span node) point)
      (se-term-expand-to (se-node-children node) point)))
    (cons
     (cons-t
      (se-term-expand-to (first node) point)
      (se-term-expand-to (rest node) point)))))
