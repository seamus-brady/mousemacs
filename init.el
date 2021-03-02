(setq gc-cons-threshold 100000000)

(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(defun load-directory (dir)
     (let ((load-it (lambda (f)
		       (load-file (concat (file-name-as-directory dir) f)))
		     ))
	(mapc load-it (directory-files dir nil "\\.el$"))))
    (load-directory "~/.emacs.d/mousemacs/")
    (load-directory "~/.emacs.d/)
