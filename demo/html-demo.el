
(require 'se-mode)
(require 'se-highlight)

(defun se-html-parse-file ()
  (interactive)
  (run-hooks 'se-inf-parse-hook)
  (se-inf-ask (concat "PARSE-FILE\t" (buffer-file-name) "\thtml")))

(find-file "example.html")
(se-mode)

(se-inf-start
 (or (get-buffer-process "*se-mode: html-demo*")
     (start-process "html-demo" "*se-mode: html-demo*"
		    "java" "-cp" "*" "InfJava")))

(setq se-highlight-font-map
      '((string . ("literal" "htmlChardata"))
	(type . ("formalParameterList"))
	(function-name . ("htmlTagName" "singleExpression")))
      )

(se-navi-define-key 'html-mode (kbd "c") #'se-html-parse-file)
(se-navi-define-key 'html-mode (kbd "h") #'se-highlight)
