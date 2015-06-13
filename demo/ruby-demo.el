
(load-file "../se-helpers.el")
(load-file "../se.el")

(defconst json-array-type 'list)
(load-file "../json.el/json.el")

(defun list-to-spans (list)
  (mapcar (lambda (x) (apply 'new-span x))
	  list))

(defconst se-parse-tree
  (create-parse-tree
   (list-to-spans (json-read-file "ruby-demo-spans"))))
(load-file "../se-mode.el")

(find-file "example.rb")
(se-mode)
