
(load-file "../se.el")
(load-file "../se-helpers.el")
(load-file "../se-mode.el")
(load-file "../se-inf.el")

(load-file "../json.el/json.el")

(defun list-to-spans (list)
  (mapcar (lambda (x) (apply 'se-new-span x))
	  list))

(defconst se-mode-demo-tree
  (let ((json-array-type 'list))
    (se-create-parse-tree
     (list-to-spans (json-read-file "java_spans")))))

(defun se-java-parse-file ()
  (interactive)
  (se-inf-ask (concat "PARSE-FILE\t" (buffer-file-name) "\tjava")))

(defun se-java-select-method ()
  (interactive)
  (se-mode-select-name "classBodyDeclaration"))

(find-file "InfJava.java")
(se-mode)
(se-inf-start
 (start-process "java-demo" "*se-mode: java-demo*"
		"java" "-cp" "*" "InfJava"))
(setq se-mode-parse-tree se-mode-demo-tree)
(add-hook 'se-inf-parse-hook #'se-java-parse-file)
(define-key se-navigation-mode-map (kbd "m") #'se-java-select-method)
