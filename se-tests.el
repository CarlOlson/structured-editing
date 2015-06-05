
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
	(cons (new-span "L4" 19 21) pos-test-data))

  (assert
   (create-parse-tree pos-test-data))

  ;; The negative test will corrupt pos-test-data due to sorting the
  ;; bad list.
  (assert
   (null
    (create-parse-tree neg-test-data)))
  
  )
