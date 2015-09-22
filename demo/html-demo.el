
(require 'se-mode)

(defun list-to-spans (list)
  (mapcar (lambda (x) (apply 'se-new-span x))
	  list))

(defconst se-mode-demo-tree
  (let ((json-array-type 'list))
    (se-create-parse-tree
     (list-to-spans (json-read-file "html_spans")))))

(defun se-html-parse-file ()
  (interactive)
  (se-inf-ask (concat "PARSE-FILE\t" (buffer-file-name) "\thtml")))

(find-file "example.html")
(se-mode)
(unless se-inf-parse-hook
  (se-inf-start
   (start-process "html-demo" "*se-mode: html-demo*"
		  "java" "-cp" "*" "InfJava")))
(setq se-mode-parse-tree se-mode-demo-tree)
(add-hook 'se-inf-parse-hook #'se-html-parse-file)
(define-key se-mode-map (kbd "<tab>") #'se-mode-indent-buffer)
