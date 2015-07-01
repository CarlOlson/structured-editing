
;; explicitly re-evaluate code
(load-file "se-helpers.el")
(load-file "se.el")

(defun se--create-test-spans ()
  (mapcar
   (curry apply 'se-new-span)
   '(("L1" 1 100)
     ("L2" 1 20)
     ("L3" 1 10)
     ("L2" 21 50)
     ("L2" 51 100)
     ("L1" 101 110)
     ("L3" 30 40)
     ("L3" 41 50))))

(defun se--create-test-tree ()
  (se-create-parse-tree (se--create-test-spans)))

;; (defmacro should-all (pred &rest args)
;;   (let ((arg (gensym)))
;;     `(dolist (,arg ',args)
;;        (should (funcall ,pred ,arg)))))

;; (defmacro should-none (pred &rest args)
;;   (let ((arg (gensym)))
;;     `(dolist (,arg ',args)
;;        (should-not (funcall ,pred ,arg)))))

(ert-deftest se-term-helpers ()
  "Test that term helper methods work."
  (let* ((span1 (se-new-span "L1" 1 10))
	 (node1 (se-new-node span1 nil)))
    (should (equal (list span1) (se-as-spans (list node1))))
    (should (equal span1 (se-first-span node1)))
    (should (equal span1 (se-last-span node1)))
    (should (equal 1 (se-term-start node1)))
    (should (equal 10 (se-term-end node1)))
    (should (equal 10 (se-term-length node1)))
    (should (se-point-in-term-p 10 node1))
    (should-not (se-point-in-term-p 0 node1))
    (should (se-term-equal-p span1 node1))
    ))

(ert-deftest se-last-span-regression ()
  "should not reach reach max eval depth"
  (should (se-last-span (se--create-test-tree)))
  )

(ert-deftest se-span-helpers ()
  "Test that se-span helpers work."
  (let ((span1 (se-new-span "L1" 1  10))
	(span2 (se-new-span "L2" 2   5))
	(span3 (se-new-span "L1" 11 20)))
    (should (se-term-before-p span1 span2))
    (should (se-term-before-p span1 span3))
    (should (se-term-before-p span2 span3))
    (should (se-term-child-p span1 span1))
    (should (se-term-child-p span2 span1))
    (should-not (se-term-child-p span2 span3))
    ))

(ert-deftest se-parse-tree ()
  "Test parse tree creation."
  (let ((bad-span (se-new-span "" 19 21)))
    (should (se--create-test-tree))
    (should-not (se-create-parse-tree
		 (cons bad-span (se--create-test-spans))))
    (should (equal (sort (se--create-test-spans) #'se-term-before-p)
		   (se-flatten (se--create-test-tree))))
    ))

(ert-deftest se-find-methods ()
  (let ((tree (se--create-test-tree))
	(span1 (se-new-span "L3" 1 10))
	(span2 (se-new-span "L3" 5 30))
	(span3 (se-new-span "L3" 30 40)))
    ;; se-find-point
    (should (= 10 (se-term-end (se-find-point 1 tree))))
    (should (se-node-p (se-find-point 1 tree)))
    ;; se-find-point-path
    (should (= 3 (length (se-find-point-path 1 tree))))
    (should (every #'se-node-p (se-find-point-path 1 tree)))
    ;; se-find-span
    (should (se-node-p (se-find-span span1 tree)))
    (should-not (se-find-span span2 tree))
    ;; se-find-span-path
    (should (= 3 (length (se-find-span-path span1 tree))))
    (should (every #'se-node-p (se-find-span-path span1 tree)))
    (should-not (se-find-span-path span2 tree))
    ;; se-find-after
    (should (= 5 (length (se-flatten (se-find-after span1 tree)))))
    ;; (should-not (se-find-after span2 tree))
    (should (= 3 (length (se-find-after span3 tree))))
    ))

(ert-deftest se-destructive-methods ()
  "Test that destructive methods function."
  ;; tests need to be written
  ;; se-node-add-offset
  ;; se-term-add-offset
  ;; se-node-expand-to
  )
