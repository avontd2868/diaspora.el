;;; diaspora-edit-mode.el --- 
;; 
;; Filename: diaspora-edit-mode.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: mié abr 25 11:23:06 2012 (-0300)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


(defvar diaspora-post-edit-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'diaspora-post-this-buffer)
    map
    ))

(defvar diaspora-post-edit-mode-keywords
  '(t (
       (diaspora-regexp-tag . diaspora-tag-face)
       (diaspora-regexp-user-name . diaspora-user-name-citation-face)
       )
      )
  "Keywords for `diaspora-post-edit-mode' minor mode."
  )
  

(define-minor-mode diaspora-post-edit-mode 
  "Minor mode for adding keymaps and highlightings according to D*."
  nil
  " D*-post-edit"
  diaspora-post-edit-mode-map
  :group 'diaspora

  (if diaspora-post-edit-mode
      (diaspora-pem-add-keywords)
    (diaspora-pem-remove-keywords)
    )	    
  )

;; "pem" = "post edit mode". As abreviation we use "pem" instead of "post-edit-mode".

(defun diaspora-pem-add-keywords ()
  "Append the `diaspora-post-edit-mode-keywords' into the `font-lock-defaults'."
  (setq font-lock-keywords 
	(append diaspora-post-edit-mode-keywords font-lock-keywords)
	)
  )

(defun diaspora-pem-remove-keywords ()
  "Remove the `diaspora-post-edit-mode-keywords' from the `font-lock-defaults'."
  (dolist (e diaspora-post-edit-mode-keywords)
    (setq font-lock-defaults (remove e font-lock-defaults))
    )
  )


(provide 'diaspora-post-edit-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diaspora-edit-mode.el ends here
