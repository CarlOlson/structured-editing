
;; explicitly re-evaluate code
(load-file "se-helpers.el")
(load-file "se.el")

(eval-when-compile
  (assert (span< (new-span "less" 1  20)
		  (new-span "more" 21 40)))

  (assert (span< 
	   (new-span "first"  1 40)
	   (new-span "second" 1 20)))

  (assert (is-span-child (new-span "child" 1 2)
			  (new-span "main" 1 10)))

  (setq pos-test-data
	(mapcar
	 (curry apply 'new-span)
	 '(("L1" 1  100)
	   ("L2" 1   20)
	   ("L2" 21  50)
	   ("L2" 51 100)
	   ("L3" 1   10)
	   ("L3" 30  40)
	   ("L3" 41  50))))

  (setq neg-test-data
	(cons (new-span "L4" 19 21) (copy-list pos-test-data)))

  (assert
   (create-parse-tree pos-test-data))

  (assert
   (null
    (create-parse-tree (copy-list neg-test-data))))

  (assert
   (every (lambda (x) (is-point-in-span x (new-span nil 1 10)))
	  '(1 5 10)))

  (assert
   (notany (lambda (x) (is-point-in-span x (new-span nil 1 10)))
	   '(0 11)))

  (assert
   (equal (find-min-span 1 (create-parse-tree pos-test-data))
	  (new-span "L3" 1 10)))

  (assert
   (equal (find-min-span 42 (create-parse-tree pos-test-data))
	  (new-span "L3" 41 50)))
  
  (assert
   (null (find-min-span 0 (create-parse-tree pos-test-data))))

  )
