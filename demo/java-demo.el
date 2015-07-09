
(load-file "../se-helpers.el")
(load-file "../se.el")

(defconst json-array-type 'list)
(load-file "../json.el/json.el")

(defun list-to-spans (list)
  (mapcar (lambda (x) (apply 'se-new-span x))
	  list))

(defconst se-mode-demo-tree
  (se-create-parse-tree
   (list-to-spans (json-read-file "java_spans"))))

(load-file "../se-mode.el")

(find-file "InfJava.java")
(se-mode)
(setq se-mode-parse-tree se-mode-demo-tree)
