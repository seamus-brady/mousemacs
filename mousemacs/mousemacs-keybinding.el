;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define mousemacs keybindings here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package bind-key :ensure t)

;; save file - this should work :)
(define-key (current-global-map) (kbd "C-s") nil)
(bind-key* (kbd "C-s") 'save-buffer)

;; select all
(define-key (current-global-map) (kbd "C-a") nil)
(bind-key* (kbd "C-a") 'mark-whole-buffer)

;; create new empty buffer
(define-key (current-global-map) (kbd "C-n") nil)
(bind-key* (kbd "C-n") 'new-empty-buffer)

;; open file
(define-key (current-global-map) (kbd "C-o") nil)
(bind-key* (kbd "C-o") 'counsel-find-file)

;; close file buffer
(define-key (current-global-map) (kbd "C-k") nil)
(bind-key* (kbd "C-k") ' kill-buffer )

;; quit
(define-key (current-global-map) (kbd "C-q") nil)
(bind-key* (kbd "C-q") 'save-buffers-kill-terminal)

;; switch windows
(define-key (current-global-map) (kbd "C-w") nil)
(bind-key* (kbd "C-w") 'switch-window)

;; swiper, counsel and ivy settings
(bind-key* (kbd "C-f") 'swiper-isearch)
;;(global-set-key (kbd "C-s") 'swiper-isearch)
(bind-key* (kbd "C-r") 'swiper)  ;; like Goto Anything
(bind-key* (kbd "M-x")  'counsel-M-x)
(bind-key* (kbd "C-S-p")  'counsel-M-x) ;; like sublimetext/vscode


;; file explorer
(bind-key*  [f8] 'neotree-toggle)
(define-key (current-global-map) (kbd "C-s") nil)
(bind-key* (kbd "C-e") 'neotree-toggle)

;; [Home] & [End] key should take you to beginning and end of lines..
(bind-key* [home] 'beginning-of-line)
(bind-key* [end] 'end-of-line)

;;; switch between buffers
(bind-key* [C-tab] 'previous-buffer)
(define-key (current-global-map) (kbd "C-b") nil)
(bind-key* (kbd "C-b") 'ivy-switch-buffer)

;; single window dired
;; use the single mode dired as provided by
;; https://github.com/jixiuf/joseph-single-dired
(load (concat mousemacs-path "resources/single-dired.el"))
(define-key (current-global-map) (kbd "C-d") nil)
(bind-key* (kbd "C-d") 'dired)
