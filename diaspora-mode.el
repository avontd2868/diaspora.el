;;; diaspora-mode.el --- 
;; 
;; Filename: diaspora-mode.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: mié abr  4 11:47:07 2012 (-0300)
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
;; This library provides functions and variables that create the major 
;; mode called `diaspora-mode'.
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

(require 'font-lock)

(defgroup diaspora-mode nil
  "`diaspora-mode' behaviour customization."
  :group 'diaspora
  :version "23.0"
  :tag "Diaspora-mode")


;; Font lock

(defgroup diaspora-faces nil
  "Faces used in diaspora Mode"
  :group 'diaspora
  :group 'faces)

;; (defcustom diaspora-regex-bare-link
;;   "http://[a-zA-Z0-9-_\./?=&]*"
;; or "^http://.*"
;;   "Regular expression for a `http://'"
;;   :type 'regexp
;;   :group 'diaspora)

(defcustom diaspora-regexp-youtube-link
  "^\\(http.*://www.youtube.com/watch\\?v=\\)\\([^\)].*\\)"
  "Regular expression for a youtube link"
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-image-alist
  "\\(`?http.://\\|\\[\\[\\|<\\|`\\)?\\([-+./_0-9a-zA-Z]+\\.\\(GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|N[GM]\\|PM\\)\\|SVG\\|TIFF?\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|n[gm]\\|pm\\)\\|svg\\|tiff?\\|x\\(?:[bp]m\\)\\)\\)\\(\\]\\]\\|>\\|'\\)?"
  "Taken from iimage-mode."
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-image
"!\\(\\[[^]]*?\\]\\)(\\(`?http.*:[^\\)?]*\\))"
  "Regular expression for a [text](file) or an image link ![text](file).
Note: this is not correct! Needs more thought to get all images right."
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-user-entry 
"^[a-zA-Z0-9_úùüãâáàéíìõóòñ\s-\.\*\/@]*[a-zA-Z0-9_úùüãâáàéíìõóòñ\s-\.\*\/@]*@[a-zA-Z0-9\s-]*[\.a-zA-Z0-9\s-]*)"
  "Regular expression for user entry."
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-tag
  "#[a-zA-Z0-9_/\.-]+"
  "Regular expression for a tag."
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-header-1
  "^\\(# \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 1"
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-header-2
  "^\\(## \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 2"
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-header-3
  "^\\(### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 3"
  :type 'regexp
  :group 'diaspora)


(defcustom diaspora-regexp-header-4
  "^\\(#### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 4"
  :type 'regexp
  :group 'diaspora)

(defconst diaspora-regexp-code
  "\\(^\\|[^\\]\\)\\(\\(`\\{1,2\\}\\)\\([^ \\]\\|[^ ]\\(.\\|\n[^\n]\\)*?[^ \\]\\)\\3\\)"
  "Regular expression for matching inline code fragments.")


(defconst diaspora-regexp-bold
  "\\(^\\|[^\\]\\)\\(\\([*_]\\{2\\}\\)\\(.\\|\n[^\n]\\)*?[^\\ ]\\3\\)"
  "Regular expression for matching bold text.")

(defconst diaspora-regexp-emph
  "\\(^\\|[^\\]\\)\\(\\([*_]\\)\\([^ \\]\\3\\|[^ ]\\(.\\|\n[^\n]\\)*?[^\\ ]\\3\\)\\)"
  "Regular expression for matching emph text.")

(defconst diaspora-regexp-email
  "<\\(\\sw\\|\\s_\\|\\s.\\)+@\\(\\sw\\|\\s_\\|\\s.\\)+>"
  "Regular expression for matching inline email addresses.")

(defconst diaspora-regexp-blockquote
  "^>.*$"
  "Regular expression for matching blockquote lines.")

(defconst diaspora-regexp-hr
  "^\\(\\*[ ]?\\*[ ]?\\*[ ]?[\\* ]*\\|-[ ]?-[ ]?-[--- ]*\\)$"
  "Regular expression for matching markdown horizontal rules.")

(defconst diaspora-regexp-buttons-elements
  "Read in new buffer"
  "Regular expression for matching buttons like \"Read in new buffer\".
This buttons are used by the user for clicking or pressing ENTER.")


(defvar diaspora-header-face-1 'diaspora-header-face-1
  "Face name to use for level-1 headers.")

(defvar diaspora-header-face-2 'diaspora-header-face-2
  "Face name to use for level-2 headers.")

(defvar diaspora-header-face-3 'diaspora-header-face-3
  "Face name to use for level-3 headers.")

(defvar diaspora-header-face-4 'diaspora-header-face-4
  "Face name to use for level-4 headers.")

(defvar diaspora-url-face 'diaspora-url-face
  "Face name to use for URLs.")

(defvar diaspora-link-face 'diaspora-link-face
  "Face name to use for links.")

(defvar diaspora-emph-face 'diaspora-emph-face
  "Face name to use for links.")

(defvar diaspora-bold-face 'diaspora-bold-face
  "Face name to use for links.")

(defvar diaspora-emph-face 'diaspora-emph-face 
  "Face name to use for links.")

(defvar diaspora-inline-code-face 'diaspora-inline-code-face
  "Face name to use for inline code.")

(defvar diaspora-blockquote-face 'diaspora-blockquote-face
  "Face name to use for blockquote text.")

(defface diaspora-inline-code-face
  '((t :inherit font-lock-constant-face))
  "Face for inline code."
  :group 'diaspora-faces)

(defface diaspora-blockquote-face
  '((t :inherit font-lock-doc-face))
  "Face for blockquote sections."
  :group 'diaspora-faces)

(defface diaspora-header-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Base face for headers."
  :group 'diaspora-faces)

(defface diaspora-header-face-1
  '((t :inherit diaspora-header-face))
  "Face for level-1 headers."
  :group 'diaspora-faces)

(defface diaspora-header-face-2
  '((t :inherit diaspora-header-face))
  "Face for level-2 headers."
  :group 'diaspora-faces)

(defface diaspora-header-face-3
  '((t :inherit diaspora-header-face))
  "Face for level-3 headers."
  :group 'diaspora-faces)

(defface diaspora-header-face-4
  '((t :inherit diaspora-header-face))
  "Face for level-4 headers."
  :group 'diaspora-faces)

(defface diaspora-link-face
  '((t :inherit font-lock-keyword-face))
  "Face for links."
  :group 'diaspora-faces)

(defface diaspora-url-face
  '((t :inherit font-lock-string-face))
  "Face for URLs."
  :group 'diaspora-faces)

(defface diaspora-emph-face
  '((t :inherit font-lock-variable-name-face :italic t))
  "Face for italic text."
  :group 'diaspora-faces)

(defface diaspora-bold-face
  '((t :inherit font-lock-variable-name-face :bold t))
  "Face for bold text."
  :group 'diaspora-faces)

(defface diaspora-link-face
  '((t :inherit font-lock-keyword-face))
  "Face for links."
  :group 'diaspora-faces)


(defface diaspora-buttons-elements-face
  '((t :weight bold :overline t ))
  "Face for buttons like \"Read in new buffer\"."
  :group 'diaspora-faces
  )

(defun diaspora-check-is-link-to-pub (limit)  
  "Return t if the text from the current point up to the limit has the property diaspora-is-link-to-public setted to t."
  (if (get-text-property (point) 'diaspora-is-link-to-pub)
      ;; Point is on a link-to-publication text!
      (let ((beg-pos (point))
	    (end-pos (next-single-property-change (point) 'diaspora-is-link-to-pub nil limit)) ;;find the last char where the property is false.	   
	    )

	(message "D*:: appling link-to-pub\n")
	
	;; Set match-data
	(set-match-data (list beg-pos end-pos))
	t
	)
    )
  )


(defcustom diaspora-mode-hook '(diaspora-see-regexp-markdow diaspora-show-videos)
  "Functions run upon entering `diaspora-mode'."
  :type 'hook
  :options '(flyspell-mode turn-on-auto-fill longlines-mode diaspora-get-all-images diaspora-show-images)
  :group 'diaspora)

(defvar diaspora-mode-font-lock-keywords
  (list
    ;;   (cons diaspora-regexp-bare-link '(2 diaspora-url-face t))
    (cons diaspora-regexp-blockquote 'diaspora-blockquote-face)
    (cons diaspora-regexp-user-entry 'diaspora-header-face-1)
    (cons diaspora-regexp-header-1 'diaspora-header-face-1)
    (cons diaspora-regexp-header-2 'diaspora-header-face-2)
    (cons diaspora-regexp-header-3 'diaspora-header-face-3)
    (cons diaspora-regexp-header-4 'diaspora-header-face-4)
    (cons diaspora-regexp-hr 'diaspora-header-face-1)
    (cons diaspora-regexp-image
         '((1 diaspora-link-face t)
           (2 diaspora-url-face t)))
    (cons diaspora-regexp-bold '(2 diaspora-bold-face))
    (cons diaspora-regexp-emph '(2 diaspora-emph-face))
    (cons diaspora-regexp-code '(2 diaspora-inline-code-face))
    (cons diaspora-regexp-email 'diaspora-link-face)
    (cons diaspora-regexp-tag 'diaspora-url-face)
    (cons diaspora-regexp-buttons-elements '(3 diaspora-buttons-elements-face))
    )
  

  
  "Syntax highlighting for diaspora files.")

(defvar diaspora-mode-map 
  (let ((diaspora-mode-map (make-sparse-keymap)))
    (define-key diaspora-mode-map "\C-c4" 'diaspora-markdown-insert-headline-4)
    (define-key diaspora-mode-map "\C-c3" 'diaspora-markdown-insert-headline-3)
    (define-key diaspora-mode-map "\C-c2" 'diaspora-markdown-insert-headline-2)
    (define-key diaspora-mode-map "\C-c1" 'diaspora-markdown-insert-headline-1)
    (define-key diaspora-mode-map "\C-c\C-cl" 'diaspora-markdown-insert-unordered-list)
    (define-key diaspora-mode-map "\C-c\C-ce" 'diaspora-markdown-insert-emph-text)
    (define-key diaspora-mode-map "\C-c\C-cb" 'diaspora-markdown-insert-bold-text)
    (define-key diaspora-mode-map "\C-c\C-c-" 'diaspora-markdown-insert-horizontal-rule)
    (define-key diaspora-mode-map "\C-c\C-ch" 'diaspora-markdown-insert-link)
    (define-key diaspora-mode-map "\C-c\C-ci" 'diaspora-markdown-insert-image)
    (define-key diaspora-mode-map "\C-c\C-cm" 'diaspora-markdown-mention-user)
    (define-key diaspora-mode-map "\C-cp" 'diaspora-post-this-buffer)
    (define-key diaspora-mode-map "\C-c\C-cp" 'diaspora-post-to)
    (define-key diaspora-mode-map "\C-c\C-cc" 'diaspora-post-clipboard)
    (define-key diaspora-mode-map "\C-c\C-k" 'diaspora-post-destroy)
    (define-key diaspora-mode-map "\C-cl" 'diaspora-toogle-images) ; not implemented yet
    (define-key diaspora-mode-map "\C-cio" 'diaspora-show-image-at-point)
    diaspora-mode-map)
  "Keymap based on html-mode")

;;;###autoload
(define-derived-mode diaspora-mode text-mode "diaspora"
  "Major mode for output from \\[diaspora*]."
  (set (make-local-variable 'font-lock-defaults)
       '(diaspora-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t)
  (use-local-map diaspora-mode-map)
  (set (make-local-variable 'buffer-read-only) t)
  (run-hooks 'diaspora-mode-hook))


(provide 'diaspora-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diaspora-mode.el ends here
