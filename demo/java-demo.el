
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

(defun se-mode-parse-file ()
  (interactive)
  (se-inf-ask (concat "PARSE-FILE\t" (buffer-file-name) "\tjava")))
(define-key se-mode-map (kbd "C-c c") #'se-mode-parse-file)

(find-file "InfJava.java")
(se-mode)
(setq se-inf-process
      (start-process "java-demo" "*se-mode: java-demo*"
		     "java" "-cp" "*" "InfJava"))
(se-inf-start)
(setq se-mode-parse-tree se-mode-demo-tree)
