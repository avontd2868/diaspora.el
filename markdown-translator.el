;;; markdown-translator.el --- 
;; 
;; Filename: markdown-translator.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: mié abr  4 23:04:36 2012 (-0300)
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

(require 'markdown-mode)

(defun markdown-trans-hide ()
  "Remove the leading and trailing \"**\" chars from all the text in the current buffer that has the property \"markdown-trans-bold\"."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (markdown-trans-hide-chars 'markdown-trans-hide)  
    )
  )

(defun markdown-trans-show ()
  "Remove the leading and trailing \"**\" chars from all the text in the current buffer that has the property \"markdown-trans-bold\"."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (markdown-trans-show-chars 'markdown-trans-hide)  
    )
  )


(defvar markdown-trans-functions-faces
  (list
    (cons markdown-regex-italic 'markdown-trans-add-italic-props)
    (cons markdown-regex-hr 'markdown-trans-add-hr-props)
    (cons markdown-regex-bold  'markdown-trans-add-bold-props)
    (cons markdown-regex-blockquote 'markdown-trans-add-blockquote-props)
    (cons markdown-regex-header-3-atx 'markdown-trans-add-header-3-atx-props)
    (cons markdown-regex-header-2-atx 'markdown-trans-add-header-2-atx-props)
    (cons markdown-regex-header-1-atx 'markdown-trans-add-header-1-atx-props)
    (cons markdown-regex-code 'markdown-trans-add-code-props)
    (cons markdown-regex-link-inline 'markdown-trans-add-link-props)
    )
  "A list of lists with the regexp the face and the function to apply.")

(defun markdown-trans-apply ()
  "Insert to all type of elements in the alist `markdown-trans-functions-faces' to its respective regexps."
  (interactive)
  (save-excursion
    (dolist (elt markdown-trans-functions-faces)
      (goto-char (point-min))
      (markdown-trans-insert-properties (car elt) (cdr elt))
      )
    )
  )

(defun markdown-trans-insert-properties (regexp remove-func)
  "Insert in all text that matchs REGEXP the face FACE as a face-font-lock property.
This function begins from current-position.

Also this calls REMOVE-FUNC whenever founds the regexps."
  (while (search-forward-regexp regexp nil t)
    (goto-char (match-end 0))
    (when remove-func
      (funcall remove-func (match-beginning 0) (match-end 0))
      )
    )  
  )

(defun markdown-trans-hide-chars (property)
  "Hide all chars setting the property invisible. This chars must have the property given by parameter PROPERTY setted to t."
  (let ((beg (text-property-any (point-min) (point-max) property t))
	(end 0)
	)
    (while (or beg 
	       (equal (point-max) beg))
      (setq end (next-single-property-change beg property))
      (add-text-properties beg end '(invisible t))
      (setq beg (text-property-any end (point-max) property t))
      )
    )
  )

(defun markdown-trans-show-chars (property)
  "Hide all chars setting the property invisible. This chars must have the property given by parameter PROPERTY setted to t."
  (let ((beg (text-property-any (point-min) (point-max) property t))
	(end 0)
	)
    (while (or beg 
		(equal (point-max) beg))
      (setq end (next-single-property-change beg property))
      (add-text-properties beg end '(invisible nil))
      (setq beg (text-property-any end (point-max) property t))
      )
    )
  )



(defun markdown-trans-add-bold-props (beg end)
  "Add the property \"markdown-trans-bold\" to the \"**\" chars."
  (add-text-properties (+ 1 beg) (+ beg 3) '(markdown-trans-hide t))
  (add-text-properties (- end 2) end '(markdown-trans-hide t))
  )

(defun markdown-trans-add-italic-props (beg end)
  "Add the property \"markdown-trans-bold\" to the \"**\" chars."
  (add-text-properties (+ 1 beg) (+ beg 2) '(markdown-trans-hide t))
  (add-text-properties (- end 1) end '(markdown-trans-hide t))
  )

(defun markdown-trans-add-bold-italic-props (beg end)
"Add the property \"markdown-trans-bold\" to the \"**\" chars."
  (add-text-properties beg (+ beg 3) '(markdown-trans-hide t))
  (add-text-properties (- end 3) end '(markdown-trans-hide t))  
  )

(defun markdown-trans-add-header-1-atx-props (beg end)
  (add-text-properties beg (+ beg 2) '(markdown-trans-hide t))
  (when (string-equal (buffer-substring (- end 1) end) "#")
    (add-text-properties (- end 1) end '(markdown-trans-hide t))
    )
  )

(defun markdown-trans-add-header-2-atx-props (beg end)
  (add-text-properties beg (+ beg 3) '(markdown-trans-hide t))
  (when (string-equal (buffer-substring (- end 2) end) "##")
    (add-text-properties (- end 2) end '(markdown-trans-hide t))
    )
  )

(defun markdown-trans-add-header-3-atx-props (beg end)
  (add-text-properties beg (+ beg 4) '(markdown-trans-hide t))
  (when (string-equal (buffer-substring (- end 3) end) "###")
    (add-text-properties (- end 3) end '(markdown-trans-hide t))
    )
  )

(defun markdown-trans-add-blockquote-props (beg end)
"Add the property \"markdown-trans-bold\" to the \"**\" chars."
  (add-text-properties beg (+ beg 1) '(markdown-trans-hide t))
  )

(defgroup markdown-trans nil
  "Markdown Transformation.
From Markdown to Emacs faces."
  :group 'applications
  :tag "Markdown Trans"
  :version "23.0")
  
(defcustom markdown-trans-goto-link-key
  "\C-cgl"
  "What key to use for following a link."
  :group 'markdown-trans
  :type 'string
  )

(defcustom markdown-trans-goto-link-fnc
  'nil
  "A function binded to the `markdown-trans-goto-link-key'."
  :type 'function
  :group 'markdown-trans)

(defvar markdown-trans-map
  (let ((markdown-trans-map (make-sparse-keymap)))
    (define-key markdown-trans-map markdown-trans-goto-link-key markdown-trans-goto-link-fnc)
    markdown-trans-map)
  "Personalizable keymap for markdown-trans.")

(defun markdown-trans-add-link-props (beg end)
  (add-text-properties beg end '(mouse-face highlight help-echo "mouse-2: Go to link." keymap markdown-trans-map))  
  (if (string-equal (buffer-substring beg (+ beg 1)) "!")
      (add-text-properties beg (+ beg 2) '(markdown-trans-hide t))
    (add-text-properties beg (+ beg 1) '(markdown-trans-hide t))
    )
  (save-excursion 
    (goto-char beg)
    (when (search-forward-regexp markdown-regex-link-inline end t)
      (add-text-properties (- (match-beginning 2) 1) (match-end 2) '(markdown-trans-hide t))
      )
    )    
  )

(defun markdown-trans-add-code-props (beg end)
  (add-text-properties (+ beg 1) (+ beg 2) '(markdown-trans-hide t))
  (add-text-properties (- end 1) end '(markdown-trans-hide t))
  )

(defun markdown-trans-add-hr-props (beg end)
  )

(provide 'markdown-translator)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markdown-translator.el ends here
