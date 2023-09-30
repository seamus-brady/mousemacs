
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load mousemacs core functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mousemacs-path
		(concat user-emacs-directory
        (convert-standard-filename "mousemacs/")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start the emacs server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; only one frame - start the server
(require 'server)
(or (server-running-p)
    (server-start))
(setq ns-pop-up-frames nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Changes for package loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))

(unless (>= emacs-major-version 27)
  (package-initialize)) ; guess what this one does ?

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help keep ~/.emacs.d clean
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package no-littering :ensure t)

;; move all config and data files to seperate dirs
(setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name "data/" user-emacs-directory))
(require 'no-littering)


;; exclude the config/data dirs from recentf
(require 'recentf)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs async function - run things in the background
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package async
  :ensure t)

(async-bytecomp-package-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set auto-compile mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-compile :ensure t)

(setq load-prefer-newer t)
(unless (>= emacs-major-version 27)
  (package-initialize))
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; suppress auto-compile buffers
(setq auto-compile-display-buffer nil)
(setq auto-compile-mode-line-counter t)

;; hide compilation buffers on finish compiling
(add-hook 'compilation-finish-functions (lambda (buf strg) (kill-buffer buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reset Emacs!!
;;; Assorted configurations that deal with core Emacs functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fset 'yes-or-no-p 'y-or-n-p)

(setq
 auto-save-default nil
 backup-inhibited t
 confirm-nonexistent-file-or-buffer nil
 create-lockfiles nil
 mouse-wheel-progressive-speed nil)

;; Allow 20MB of memory (instead of 0.76MB default) before calling
;; garbage collection. This means GC runs less often, which speeds
;; up some operations
(setq gc-cons-threshold 20000000)

;; mru file menu
(recentf-mode t)
(setq recentf-max-menu-items 25)

;; don't load elpa files in recentf
(add-to-list 'recentf-exclude
             (concat user-emacs-directory
                     (convert-standard-filename "elpa/")))

(add-to-list 'recentf-exclude
             (concat user-emacs-directory
                     (convert-standard-filename "var/")))
(add-to-list 'recentf-exclude
             (concat user-emacs-directory
			(convert-standard-filename "etc/")))

;; turn off bleeps!
(setq visible-bell 1)

;; Prevent the bell from ringing all the time.
(defcustom mode-line-bell-string "ding" ; "♪"
  "Message displayed in mode-line by `mode-line-bell' function."
  :group 'user)
(defcustom mode-line-bell-delay 0.1
  "Number of seconds `mode-line-bell' displays its message."
  :group 'user)

;; turn off bell internal variables
(defvar mode-line-bell-cached-string nil)
(defvar mode-line-bell-propertized-string nil)

;; Briefly display a highlighted message in the mode-line.
(defun mode-line-bell ()
  (unless (equal mode-line-bell-string mode-line-bell-cached-string)
    (setq mode-line-bell-propertized-string
          (propertize
           (concat
            (propertize
             "x"
             'display
             `(space :align-to (- right ,(string-width mode-line-bell-string))))
            mode-line-bell-string)
           'face '(:background "black")))
    (setq mode-line-bell-cached-string mode-line-bell-string))
  (message mode-line-bell-propertized-string)
  (sit-for mode-line-bell-delay)
  (message ""))

;; mode line bell instead
(setq ring-bell-function 'mode-line-bell)

;; set utf8 everywhere
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

 ;; set bar cursor
(setq-default cursor-type 'bar)

;; set default mode for new buffers to text
(setq-default major-mode 'text-mode)

;: toggle wrapping text at the 80th character
(setq default-fill-column 80)

; hide splash screen
(setq inhibit-startup-message t)

;; turn off scratch buffer message
(setq initial-scratch-message "")

;; show parens mode all the time
(show-paren-mode 1)

;; Disable tab indentation
(setq-default indent-tabs-mode nil)

;; Remove trailing whitespace before save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Typed text will replace a highlighted region
(delete-selection-mode 1)

;; By default, backspace on Emacs turns a tab character into a set of spaces
;; & deletes one. This sets backspace to delete 1 character instead of 1 column.
(global-set-key (kbd "DEL") 'backward-delete-char)

;; Enable `downcase-region' and `upcase-region'
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; line numbers
(global-linum-mode t)
(setq linum-format " %3d ")

;; reload a file if changed in external program
(global-auto-revert-mode t)

;; change frame title to full path
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; turn on word wrap globally
(global-visual-line-mode t)

;; set a file for customisationsettings
(setq custom-file (concat user-emacs-directory "/custom.el"))
(load custom-file 'noerror)

;; start with a maximised window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GUI Hooks here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gui-mode-hook nil
  "Hook for when Emacs is executed with a GUI/Windowing system.")

(defun gui-mode-initialize ()
  (when (display-graphic-p)
    (run-hooks 'gui-mode-hook)))

(with-eval-after-load "init"
  (gui-mode-initialize))

;;; When Emacs is configured to use a gui, File->Open File will pop up
;;; a file selection dialog if `use-dialog-box' is set to t.
(setq use-dialog-box t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mouse related changes here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scroll one line at a time (less "jumpy" than defaults)
;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 3) ;; keyboard scroll a few lines at a time

;; turn on standard copy/paste behaviour
(cua-mode t)

;; Don't tabify after rectangle commands
(setq cua-auto-tabify-rectangles nil)

;; No region when it is not highlighted
(transient-mark-mode t)

;; Standard Windows behaviour
(setq cua-keep-region-after-copy t)

;; turn off mouse mode that interfere
(setq mouse-yank-at-point nil)
(setq mouse-drag-copy-region nil)
(setq x-select-enable-primary nil)
(setq x-select-enable-clipboard t)
(setq select-active-regions t)

;; turn off middle mouse button
(global-set-key (kbd "<mouse-2>") nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Changes related to buffers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create new empty buffer
(defun new-empty-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "Untitled")))
    (switch-to-buffer buf)
    (normal-mode 1)
    (linum-mode 1)
    (put 'buffer-offer-save 'permanent-local t)
    (setq buffer-offer-save t)))

;; prompt if non-file buffer has changes before killing
(defadvice kill-buffer (around kill-buffer-ask activate)
  "If `buffer-offer-save' is non-nil and a buffer is modified,
prompt before closing."
  (if (and buffer-offer-save (buffer-modified-p))
      (when (yes-or-no-p "The buffer isn't saved. Quit? ")
        ad-do-it)
    ad-do-it))

;; close all other buffers
(defun kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; close the scratch buffer on startup
(defun daisymacs-close-scratch ()
  (kill-buffer "*scratch*")
  (if (not (delq nil (mapcar 'buffer-file-name (buffer-list))))
      (new-untitled-buffer)))

(defun daisymacs-emacs-startup-hook ()
  (daisymacs-close-scratch))
(add-hook 'emacs-startup-hook 'daisymacs-emacs-startup-hook)

;; open a new empty buffer
(defun new-untitled-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "Untitled")))
    (switch-to-buffer buf)
    (normal-mode)
    (setq buffer-offer-save t)))

;; set initial mode to be text
(setq initial-major-mode 'text-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Autocomplete for mousemacs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
)

;; enable auto-complete using company
(add-hook 'after-init-hook 'global-company-mode)

(global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package undo-tree options for fancy undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global spell check.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-pos-tip
  :ensure t)

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highlight current line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hlinum
  :ensure t
  :init (hlinum-activate))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highlight cursor position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package beacon
  :ensure t
  :init (beacon-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; An extension to the Emacs text editor that automatically pairs braces and quotes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start electric-pair everywhere
(electric-pair-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highlights delimiters such as parentheses according to their depth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rainbow-delimiters
  :ensure t)

;; turn on for programming langs
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Collection of Ridiculously Useful eXtensions for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package crux
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Very large file support for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vlf
  :ensure t)

(require 'vlf-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Switch windows with numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package switch-window
  :ensure t)

(require 'switch-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ivy is an interactive interface for completion in Emacs
;;; Counsel and Swiper are included here as they are related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :ensure t
  :init
  (ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

;; add some support packages
(use-package counsel :ensure t)
(use-package swiper :ensure t)

;; install smex for sorting commands
(use-package smex
  :ensure t
  :init
  (smex-initialize))

;; show methods and code structure
(use-package imenu-anywhere :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clean up emacs windows with a popup window manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package popwin
  :ensure t
  :config
  (popwin-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define mousemacs windowing changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add window dividers
(window-divider-mode)

;; add winner mode to save window config settings
(winner-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Right click menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package popup-edit-menu
  :ensure t
  :config
  (global-set-key [mouse-3] (popup-edit-menu-stub)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NeoTree file explorer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package neotree
  :ensure t
  :config
  (setq neo-window-width 45)
  ;; auto open on current file
  (setq neo-smart-open t)
  ;; hide hidden files
  (setq-default neo-show-hidden-files nil))

;; show the file explorer at startup
;;(neotree-toggle)

(tool-bar-add-item "home" 'neotree-toggle
               'Files
               :help   "Toggle File Explorer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tabbar for mousemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tabbar
        :ensure t
  :config
  (tabbar-mode t))

;; tabs by default
(tabbar-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mousemacs tabbar config.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Just one tab group for all tabs
(setq tabbar-buffer-groups-function
         (lambda ()
             (list "All")))


 ; Add a buffer modification state indicator in the tab label, and place a
;; space around the label to make it looks less crowd.
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value
  (if (and (buffer-modified-p (tabbar-tab-value tab))
     (buffer-file-name (tabbar-tab-value tab)))
      (concat " + " (concat ad-return-value " "))
    (concat " " (concat ad-return-value " ")))))

;; Called each time the modification state of the buffer changed.
(defun daisymacs-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))

;; First-change-hook is called BEFORE the change is made.
(defun daisymacs-on-buffer-modification ()
  (set-buffer-modified-p t)
  (daisymacs-modification-state-change))
(add-hook 'after-save-hook 'daisymacs-modification-state-change)

(add-hook 'first-change-hook 'daisymacs-on-buffer-modification)

;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "  [%s]  " (tabbar-tab-tabset tab))
                  (format "  %s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

;; ------------------

(defun tabbar-popup-menu ()
  "Keymap for pop-up menu.  Emacs only."
  `(,(format "%s" (nth 0 tabbar-last-tab))
    ["Close Tab" tabbar-popup-close]
    ["Close All Other Tabs" kill-other-buffers]
    "--"
    ["Save" tabbar-popup-save]
    ["Save As" tabbar-popup-save-as]
    "--"
    ["New Empty Tab" new-empty-buffer]
    "--"
    ["Open File" tabbar-open-file ]
    "--"
    ["Copy Full Path to Clipboard" tabbar-popup-copy-path
     :active (and (buffer-file-name (tabbar-tab-value tabbar-last-tab))
                  (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab))))
     ]
     ["Copy Directory Path" tabbar-popup-copy-dir
     :active (and (buffer-file-name (tabbar-tab-value tabbar-last-tab))
                  (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab))))
     ]
    ["Copy File Name" tabbar-popup-copy-file
     :active (and (buffer-file-name (tabbar-tab-value tabbar-last-tab))
                  (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab))))
     ]
    "--"
    ["Split Window Top/Bottom" split-window-below]
    ["Split Window Left/Right" split-window-right]
    ["View Only Current Window" delete-other-windows]
    "--"
    ["Open Emacs Shell" shell]
    "--"
    ["Toggle File Explorer" neotree-toggle ]
    "--"
    ["Open Buffer List" buffer-menu ]
    ))

(defun tabbar-open-file ()
  "Edit the existing file FILENAME."
  (interactive)
  (let* ((mustmatch (not (and (fboundp 'x-uses-old-gtk-dialog)
            (x-uses-old-gtk-dialog))))
   (filename (car (find-file-read-args "Find file: " mustmatch))))
    (if mustmatch
  (find-file-existing filename)
      (find-file filename))))

(defun tabbar-popup-close ()
  "Tabbar pop up close."
  (interactive)
  (funcall tabbar-close-tab-function tabbar-last-tab))

(defun tabbar-popup-save-as ()
  "Tabbar save as."
  (interactive)
  (let* ((buf (tabbar-tab-value tabbar-last-tab)))
    (with-current-buffer buf
      (call-interactively 'write-file))))


(defun tabbar-popup-copy-path ()
  "Tabbar copy path."
  (interactive)
  (let* ((buf (tabbar-tab-value tabbar-last-tab))
         (fn (buffer-file-name buf)))
    (kill-new fn)))


(defun tabbar-popup-copy-file ()
  "Tabbar copy file name."
  (interactive)
  (let* ((buf (tabbar-tab-value tabbar-last-tab))
         (fn (buffer-file-name buf)))
    (kill-new (file-name-nondirectory fn))))


(defun tabbar-popup-copy-dir ()
  "Tabbar copy directory."
  (interactive)
  (let* ((buf (tabbar-tab-value tabbar-last-tab))
         (fn (buffer-file-name buf)))
    (kill-new (file-name-directory fn))))


(defun tabbar-context-menu ()
  "Pop up a context menu."
  (interactive)
  (popup-menu (tabbar-popup-menu)))

(defsubst tabbar-click-on-tab (tab &optional type action)
  "Handle a mouse click event on tab TAB.
Call `tabbar-select-tab-function' with the received, or simulated
mouse click event, and TAB.
Optional argument TYPE is a mouse click event type (see the function
`tabbar-make-mouse-event' for details)."
  (let* ((mouse-event (tabbar-make-mouse-event type))
         (mouse-button (event-basic-type mouse-event))
   tmp map)
    (cond
   ((eq mouse-button 'mouse-3)
    (setq tabbar-last-tab tab)
    (tabbar-context-menu))
       ((eq action 'close-tab)
  (when (and (eq mouse-button 'mouse-1) tabbar-close-tab-function)
    (funcall tabbar-close-tab-function tab)))
       ((and (eq action 'icon) (setq tmp (key-binding [menu-bar languages])))
  (with-current-buffer (tabbar-tab-value tab)
    (setq map (copy-keymap tmp)
    tmp (mouse-menu-major-mode-map))
    (define-key map [major-mode-sep-b] '(menu-item  "---"))
    (define-key map [major-mode] (cons (nth 1 tmp) tmp))
    ;; (setq tmp (make-composed-map tmp (mouse-menu-major-mode-map)))
    ;; (popup-menug tmp)
    (popup-menu map))
  (tabbar-ruler-modification-state-change)
  (tabbar-display-update))
       (t (when tabbar-select-tab-function
      (funcall tabbar-select-tab-function
         (tabbar-make-mouse-event type) tab)
      (tabbar-display-update))))))

(defun tabbar-select-tab-callback (event)
  "Handle a mouse EVENT on a tab.
Pass mouse click events on a tab to `tabbar-click-on-tab'."
  (interactive "@e")
  (cond
    ((tabbar-click-p event)
      (let ((target (posn-string (event-start event))))
        (tabbar-click-on-tab
          (get-text-property (cdr target) 'tabbar-tab (car target))
          event
          (get-text-property (cdr target) 'tabbar-action (car target)))))
    ((tabbar-drag-p event)
      (let ((start-target (posn-string (event-start event)))
            (end-target (posn-string (event-end event))))
        (tabbar-drag-tab
          (get-text-property (cdr start-target) 'tabbar-tab (car start-target))
          (get-text-property (cdr end-target) 'tabbar-tab (car end-target))
          event)))
  ))

(defsubst tabbar-drag-p (event)
  "Return non-nil if EVENT is a mouse drag event."
  (memq 'drag (event-modifiers event)))


(defun tabbar-drag-tab (dragged-tab dropped-tab event)
  "Handle DRAGGED-TAB dragged-and-dropped onto DROPPED-TAB.
   Include full mouse EVENT from drag-and-drop action."
  (let ((start-tabset (tabbar-tab-tabset dragged-tab)))
    (when (and (eq start-tabset (tabbar-tab-tabset dropped-tab))
           (not (eq dragged-tab dropped-tab)))
      (let* ((tabs (tabbar-tabs start-tabset))
         (drop-tail-length (length (memq dropped-tab tabs)))
         (drag-tail-length (length (memq dragged-tab tabs)))
         (dragdrop-pair (list dragged-tab dropped-tab))
         new-tablist)
    (when (> drag-tail-length drop-tail-length)
      (setq dragdrop-pair (reverse dragdrop-pair)))
    (dolist (thistab (reverse tabs))
      ;; build list of tabs.  When we hit dragged-tab, don't append it.
      ;; When we hit dropped-tab, append dragdrop-pair
      (cond
        ((eq thistab dragged-tab))
        ((eq thistab dropped-tab)
         (setq new-tablist (append dragdrop-pair new-tablist)))
        (t (add-to-list 'new-tablist thistab))
      ))
    (set start-tabset new-tablist)
    ;; (setq tabbar-window-cache nil)  ;; didn't help
    (tabbar-set-template start-tabset nil)
    ;; open the dragged tab
    (funcall tabbar-select-tab-function
             (tabbar-make-mouse-event event) dragged-tab)
    (tabbar-display-update)
    ))))

(defvar tabbar-close-tab-function nil
  "Function to call to close a tabbar tab.  Passed a single argument, the tab
construct to be closed.")

(defvar tabbar-new-tab-function nil
  "Function to call to create a new buffer in tabbar-mode.  Optional single
argument is the MODE for the new buffer.")

;; for buffer tabs, use the usual command to close/kill a buffer
(defun tabbar-buffer-close-tab (tab)
  (let ((buffer (tabbar-tab-value tab)))
    (with-current-buffer buffer
      (kill-buffer buffer))))

(setq tabbar-close-tab-function 'tabbar-buffer-close-tab)

(defvar tabbar-last-tab nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set up some whitespace defaults.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add show whitespace-mode - used in context menu
(use-package whitespace :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define mousemacs context menu.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package popup-edit-menu
        :ensure t
        :config
        (global-set-key [mouse-3] (popup-edit-menu-stub)))

;; NeoTree menu
(easy-menu-add-item nil '("edit") ["---" nil t])
(easy-menu-add-item nil '("edit") (list
        "File Explorer"
        ["Toggle On/Off" neotree-toggle ]
        ["Open in Current File Directory" neotree-find ]
        ["Toggle Show Hidden Files" neotree-hidden-file-toggle ]
        ["Minimise / Maximise" neotree-stretch-toggle ]
        ["Refresh" neotree-refresh ]))

;; Window menu
(easy-menu-add-item nil '("edit") (list
        "Window"
        ["Split Window Top/Bottom" split-window-below]
        ["Split Window Left/Right" split-window-right]
        ["View Only Current Window" delete-other-windows]
        ["Close Current Window" delete-window]
        ["Close Current Window (and Buffer)" kill-buffer-and-window]))

;; Buffer menu
(easy-menu-add-item nil '("edit") (list
        "Buffer Tools"
        ["Buffer List" buffer-menu]
        ["Recent File List" recentf-open-files ]))

;; Code tools
(easy-menu-add-item nil '("edit") (list
        "Coding Utilities"
        ["Toggle Visual Line Mode" visual-line-mode]
        ["Toggle Truncate Lines Mode" toggle-truncate-lines]
        ["Toggle Show Whitespace Chars" whitespace-mode ]))


;; Emacs Tools menu
(easy-menu-add-item nil '("edit") (list
        "Emacs Tools"
        ["Emacs Web Browser" eww ]))

;; end
(easy-menu-add-item nil '("edit") ["---" nil ])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Better key setting display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :ensure t
  :init
  (which-key-mode))
