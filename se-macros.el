
(defmacro se-create-mode (mode parent &rest body)
  ""
  (declare (indent 2) (debug t))
  `(progn
     (define-derived-mode
       ,(intern (downcase (format "se-%s-mode" mode)))
       ,(or parent
	    ;; keep emacs version <24 compatability
	    (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))
       ,(format "se-%s" mode)
       ,(format "Major mode for %s files." mode)
       (se-mode)
       (add-hook 'se-navigation-mode-hook
		 ',(intern (format "se-%s-parse-file" mode)) nil t)
       ,@body)
     
     (defun ,(intern (format "se-%s-parse-file" mode)) ()
       (when se-navigation-mode
	 (se-inf-parse-file)))))

(provide 'se-macros)
