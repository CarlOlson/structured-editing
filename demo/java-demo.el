
(require 'se-mode)

(defun se-java-parse-file ()
  (interactive)
  (run-hooks 'se-inf-parse-hook)
  (se-inf-ask (concat "PARSE-FILE\t" (buffer-file-name) "\tjava")))

(defun se-java-select-method ()
  (interactive)
  (se-mode-select-name "classBodyDeclaration"))

(find-file "InfJava.java")
(se-mode)

(se-inf-start
 (or (get-buffer-process "*se-mode: html-demo*")
     (start-process "java-demo" "*se-mode: java-demo*"
		    "java" "-cp" "*" "InfJava")))

(add-hook 'se-navigation-mode-hook #'se-java-parse-file)
(se-navi-define-key 'java-mode (kbd "c") #'se-java-parse-file)
(se-navi-define-key 'java-mode (kbd "m") #'se-java-select-method)
