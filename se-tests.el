
(load-file "se.el")

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

  ;; The negative test will corrupt pos-test-data due to sorting the
  ;; bad list.
  (assert
   (null
    (create-parse-tree neg-test-data)))
  
  )
