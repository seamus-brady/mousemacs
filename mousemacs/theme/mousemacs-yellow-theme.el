;;; mousemacs-theme.el
;; Copyright (C) 2018 by Seamus Brady
;; Author: Seamus Brady <seamus@corvideon.ie>
;; URL: https://www.mousemacs.org
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Default mousemacs color theme

;;; Code:

(deftheme mousemacs-yellow
  "mousemacs-yellow theme")

(custom-theme-set-faces
 'mousemacs-yellow

 '(default ((t (:background "#FFFFE6" :foreground "Black"))))
 '(mouse ((t (:foreground "Grey15"))))
 '(cursor ((t (:background "grey15"))))

 '(font-lock-comment-face ((t (:italic t :foreground "Grey55"))))
 '(font-lock-string-face ((t (:foreground "DarkMagenta"))))
 '(font-lock-keyword-face ((t (:foreground "NavyBlue"))))
 '(font-lock-warning-face ((t (:bold t :foreground "VioletRed"))))
 '(font-lock-constant-face ((t (:foreground "Blue"))))
 '(font-lock-type-face ((t (:foreground "NavyBlue"))))
 '(font-lock-variable-name-face ((t (:foreground "DarkGreen"))))
 '(font-lock-function-name-face ((t (:foreground "SlateBlue"))))
 '(font-lock-builtin-face ((t (:foreground "NavyBlue"))))
 '(highline-face ((t (:background "Grey95"))))
 '(show-paren-match-face ((t (:background "Grey80"))))
 '(region ((t (:background "Grey80"))))
 '(highlight ((t (:background "LightSkyBlue"))))
 '(secondary-selection ((t (:background "Grey55"))))
 '(widget-field-face ((t (:background "white"))))
 '(widget-single-line-field-face ((t (:background "white")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'mousemacs-yellow)

;;; mousemacs-yellow-theme.el ends here
