
(defvar se-navi-keymaps nil
  "Association list for mapping major modes to navigation mode
key bindings. Should not be accessed directly.")

(defvar se-navi-nothing-map
  (let* ((keymap (make-sparse-keymap)))
    ;; all printable characters
    (suppress-keymap keymap t)
    ;; tab, backspace, enter
    (define-key keymap (kbd "<tab>") #'se-navi-nothing)
    (define-key keymap (kbd "DEL") #'se-navi-nothing)
    (define-key keymap (kbd "RET") #'se-navi-nothing)
    ;; prevent quoted inserts
    (define-key keymap [remap quoted-insert] #'se-navi-nothing)
    keymap)
  "A keymap to make a buffer weakly read-only.")

(define-minor-mode se-navigation-mode
  "Toggle Structure Editing's Navigation mode.
\\{se-navigation-mode-map}"
  :init-value nil
  :lighter " navi"
  :keymap (let ((map (make-sparse-keymap se-navi-nothing-map)))
	    (define-key map (kbd "c") #'se-inf-parse-file)
	    (define-key map (kbd "q") #'se-navigation-mode-quit)
	    (define-key map (kbd "e") #'se-mode-expand-selected)
	    (define-key map (kbd "s") #'se-mode-shrink-selected)
	    (define-key map (kbd "i") #'se-mode-inspect)
	    (define-key map (kbd "p") #'se-mode-select-previous)
	    (define-key map (kbd "n") #'se-mode-select-next)
	    map)
  (when se-navigation-mode ;; activation
    ;; setup major-mode specific keybindings
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push (cons 'se-navigation-mode (se-navi-get-keymap major-mode))
	  minor-mode-overriding-map-alist)
    (setq se-navigation-mode-map (se-navi-get-keymap major-mode)))
  (unless se-navigation-mode ;; deactivation
    (kill-local-variable 'minor-mode-overriding-map-alist)))

(defun se-navigation-mode-quit ()
  "Quits navigation mode."
  (interactive)
  (se-navigation-mode -1))

(defun se-navi-nothing ()
  "Does nothing. Used in navigation mode keymaps."
  (interactive))

(defun se-navi-define-key (MODE KEY DEF)
  "When activating se-navigation mode in a buffer, activate some
specific bindings for your major mode.

MODE is a symbol to be matched to the value of `major-mode'.  KEY
and DEF work the same as with `define-key'."
  (let ((keymap (se-navi-get-keymap MODE)))
    (define-key keymap KEY DEF)))

(defun se-navi-get-keymap (MODE)
  (or (cdr (assoc MODE se-navi-keymaps))
      (let* ((keymap (make-sparse-keymap))
	     (entry (cons MODE keymap)))
	(set-keymap-parent keymap se-navigation-mode-map)
	(add-to-list 'se-navi-keymaps entry)
	(cdr entry))))

(provide 'se-navi)
