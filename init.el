;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

(defvar mousemacs-path
		(concat user-emacs-directory
        (convert-standard-filename "mousemacs/")))

;; load mousemacs
(load (concat mousemacs-path "mousemacs-core.el"))
(load (concat mousemacs-path "mousemacs-theme.el"))
(load (concat mousemacs-path "mousemacs-keybinding.el"))
(load (concat mousemacs-path "mousemacs-etc.el"))
