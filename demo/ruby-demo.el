
(load-file "../se-helpers.el")
(load-file "../se.el")

(defconst json-array-type 'list)
(load-file "../json.el/json.el")

(defun list-to-spans (list)
  (mapcar (lambda (x) (apply 'se-new-span x))
	  list))

(defconst se-mode-parse-tree
  (se-create-parse-tree
   (list-to-spans (json-read-file "ruby-demo-spans"))))
(load-file "../se-mode.el")

(find-file "example.rb")
(se-mode)
