
(defmacro se-create-mode (name parent &rest body)
  "A macro for defining structured editing based modes.  This
macro should contain best practices simply at the cost of
customization.

NAME should be your mode's name, properly capitalized.
PARENT should be either nil or a mode to derive the new mode
from.
BODY should contain any code to be executed when the mode starts.
It is expected that `se-inf-start' is called.

Example:
  (se-create-mode \"Ruby\" ruby-mode
    (se-inf-start ...))"
  (declare (indent 2) (debug t))
  (cl-macrolet ((idf (&rest args)
		     `(intern (downcase (format ,@args)))))
    `(progn
       (define-derived-mode
	 ,(idf "se-%s-mode" name)
	 ,(or parent
	      ;; keep emacs version <24 compatability
	      (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))
	 ,(format "se-%s" name)
	 ,(format "Major mode for %s files." name)
	 (se-mode)
	 (add-hook 'se-navigation-mode-hook
		   ',(idf "se-%s-parse-file" name) nil t)
	 ,@body)

       (if (not (fboundp ',(idf "%s-mode" name)))
	   (defalias
	     ',(idf "%s-mode" name)
	     ',(idf "se-%s-mode" name)))
       
       (defun ,(idf "se-%s-parse-file" name) ()
	 "Only parses when navigation mode is active. This prevents
the navigation mode hook from calling `se-inf-parse-file' when
deactivating."
	 (when se-navigation-mode
	   (se-inf-parse-file))))))

(provide 'se-macros)
