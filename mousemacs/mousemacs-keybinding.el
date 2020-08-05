;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define mousemacs keybindings here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; select all
(define-key (current-global-map) (kbd "C-a") nil)
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; create new empty buffer
(define-key (current-global-map) (kbd "C-n") nil)
(global-set-key (kbd "C-n") 'new-empty-buffer)

;; save file
(define-key (current-global-map) (kbd "C-s") nil)
(global-set-key (kbd "C-s") 'save-buffer)

;; open file
(define-key (current-global-map) (kbd "C-o") nil)
(global-set-key (kbd "C-o") 'counsel-find-file)

;; quit
(define-key (current-global-map) (kbd "C-q") nil)
(global-set-key (kbd "C-q") 'save-buffers-kill-terminal)

;; switch windows
(define-key (current-global-map) (kbd "C-w") nil)
(global-set-key (kbd "C-w") 'switch-window)

;; swiper, counsel and ivy settings
(global-set-key (kbd "C-f") 'swiper)
(global-set-key (kbd "M-x")  'counsel-M-x)

;; file explorer
(global-set-key  [f8] 'neotree-toggle)
(define-key (current-global-map) (kbd "C-s") nil)
(global-set-key (kbd "C-e") 'neotree-toggle)

;; [Home] & [End] key should take you to beginning and end of lines..
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

;;; switch between buffers
(global-set-key [C-tab] 'previous-buffer)
(define-key (current-global-map) (kbd "C-b") nil)
(global-set-key (kbd "C-b") 'ivy-switch-buffer)
