;;; diaspora.el --- Simple Emacs-based client for diaspora*

;; Author: Tiago Charters de Azevedo <tca@diale.org>
;; Maintainer: Tiago Charters de Azevedo <tca@diale.org>
;; Created: Jan 16, 2012
;; Version: .0
;; Keywords: diaspora*
;; URL: http://diale.org/diaspora.html

;; Copyright (c) 2011 Tiago Charters de Azevedo, Christian Giménez
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; A diaspora* client for emacs

;; Save all the files in a DIR and add that DIR to `load-path'; 
;; for instance `(add-to-list 'load-path "~/emacs.el/disaspora.el/")' to your .emacs
;; Files: diaspora.el, diaspora-post.el  and diaspora-stream.el 

(require 'url)
(require 'url-http)
(require 'json)
(require 'font-lock)

(require 'diaspora-post)
(require 'diaspora-stream)

(defconst diaspora-el-version ".0"
  "This version of diaspora*-el.")

(defgroup diaspora nil 
  "A mode for diaspora* stream view and posting."
  :group 'applications)

;;; User variable:

(defcustom diaspora-pod 
  "joinsdiaspora.com"
  "Diaspora* pod."
  :type 'string
  :group 'diaspora)


(defcustom diaspora-mode-hook nil
  "Functions run upon entering `diaspora-mode'."
  :type 'hook
  :options '(flyspell-mode turn-on-auto-fill markdown-mode)
  :group 'diaspora)

(defcustom diaspora-username nil
  "Username to use for connecting to diaspora.
If nil, you will be prompted."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'diaspora)

(defcustom diaspora-password nil
  "Password to use for connecting to diaspora.
If nil, you will be prompted."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'diaspora)

(defcustom diaspora-sign-in-url 
  "https://joindiaspora.com/users/sign_in"
  "URL used to signing in."
  :group 'diaspora)

(defcustom diaspora-status-messages-url 
  "https://joindiaspora.com/status_messages"
  "URL used to update diaspora status messages."
  :group 'diaspora)

(defcustom diaspora-entry-stream-url 
  "https://joindiaspora.com/stream.json"
  "JSON version of the entry stream(the main stream)."
  :group 'diaspora)


(defcustom diaspora-entry-file-dir
  "~/public_html/diaspora.posts/"
  "Directory where to save posts made to diaspora*."
  :group 'diaspora)

(defcustom diaspora-data-file
  "~/.diaspora"
  "Name of the file do save posts made to diaspora*."
  :type 'file
  :group 'diaspora)

(defcustom diaspora-header-post
  "## "
  "Header for each post:"
  :type 'string
  :group 'diaspora)

(defcustom diaspora-footer-post
  "#diaspora-el"
  "Footer for each post."
  :type 'string
  :group 'diaspora)

(defcustom diaspora-save-after-posting t
  "*Non-nil means automatically save after posting."
  :type 'boolean
  :group 'diaspora)

;;; Internal Variables:

;(defvar diaspora-auth-token nil
;  "")

(defvar diaspora-stream-buffer "*diaspora stream*"
  "The name of the diaspora stream buffer.")

(defvar diaspora-post-buffer "*diaspora post*"
  "The name of the diaspora post buffer.")


;;; User Functions:

;; (defun diaspora-create-file-post ()
;;   (interactive)
;;   (read-from-minibuffer "Find file: "
;; 			nil nil nil 'diaspora-post-file-name)
;;   (let ((post-buffer (get-buffer-create (car diaspora-post-file-name))))
;;     (switch-to-buffer post-buffer)
;;     (diaspora-mode)))



(defun diaspora-ask ()
  "Ask for username and password 
if `diaspora-username' and  `diaspora-password' 
has not been setted."
  (unless (and diaspora-username diaspora-password)
    (read-from-minibuffer "username: "
			  (car diaspora-username)
			  nil nil
			  'diaspora-username)
    (read-from-minibuffer "password: "
			  (car diaspora-password)
			  nil nil
			  'diaspora-password)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (define-key diaspora-mode-map "\C-c\C-cm" ' diaspora-markdown-mention-user)
    (define-key diaspora-mode-map "\C-cp" 'diaspora-post-this-buffer)
    (define-key diaspora-mode-map "\C-cl" 'diaspora-toogle-highlight) ; not implemente yet
    diaspora-mode-map)
  "Keymap based on html-mode")



(define-skeleton diaspora-markdown-insert-headline-1
  "Headline 1."
  "Text: "
  "# " str \n \n)

(define-skeleton diaspora-markdown-insert-headline-2
  "Headline 2."
  "Text: "
  "## " str \n \n)

(define-skeleton diaspora-markdown-insert-headline-3
  "Headline 3."
  "Text: "
  "### " str \n \n)

(define-skeleton diaspora-markdown-insert-headline-4
  "Headline 4."
  "Text: "
  "#### " str \n \n)

(define-skeleton diaspora-markdown-insert-unordered-list
  "Unordered list."
  "Text: "
  "* " str \n \n)

(define-skeleton diaspora-markdown-insert-emph-text
  "Emphasis."
  "Text: "
  "*" str "*")

(define-skeleton diaspora-markdown-insert-bold-text
  "Bold."
  "Text: "
  "**" str "**")

(define-skeleton diaspora-markdown-insert-horizontal-rule
  "Horizontal rule tag."
  nil
  "---" \n \n)

(define-skeleton diaspora-markdown-insert-link
  "Link"
  "Text: "
  "[" str "](http://" _ ")")

(define-skeleton diaspora-markdown-insert-image
  "Image with URL."
  "Text: "
  "![" str "](http://" _ ")")

(define-skeleton diaspora-markdown-mention-user
  "Mention user."
  "User: "
  "@{" str ";" _ (concat "@" diaspora-pod "}"))


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

(defcustom diaspora-regex-image
  "\\(!?\\[[^]]*?\\]\\)\\(([^\\)]*)\\)"
  "Regular expression for a [text](file) or an image link ![text](file)."
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regex-user-entry 
"^[a-zA-Z0-9_úùüãâáàéíìõóòñ\s-\.\*]*[a-zA-Z0-9_úùüãâáàéíìõóòñ\s-\.\*]*@[a-zA-Z0-9\s-]*[\.a-zA-Z0-9\s-]*)"
  "Regular expression for user entry."
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regex-tag
  "#[a-zA-Z0-9_/\.]+"
  "Regular expression for a tag."
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regex-header-1
  "^\\(# \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 1"
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regex-header-2
  "^\\(## \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 2"
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regex-header-3
  "^\\(### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 3"
  :type 'regexp
  :group 'diaspora)


(defcustom diaspora-regex-header-4
  "^\\(#### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 4"
  :type 'regexp
  :group 'diaspora)

(defconst diaspora-regex-code
  "\\(^\\|[^\\]\\)\\(\\(`\\{1,2\\}\\)\\([^ \\]\\|[^ ]\\(.\\|\n[^\n]\\)*?[^ \\]\\)\\3\\)"
  "Regular expression for matching inline code fragments.")


(defconst diaspora-regex-bold
  "\\(^\\|[^\\]\\)\\(\\([*_]\\{2\\}\\)\\(.\\|\n[^\n]\\)*?[^\\ ]\\3\\)"
  "Regular expression for matching bold text.")

(defconst diaspora-regex-emph
  "\\(^\\|[^\\]\\)\\(\\([*_]\\)\\([^ \\]\\3\\|[^ ]\\(.\\|\n[^\n]\\)*?[^\\ ]\\3\\)\\)"
  "Regular expression for matching emph text.")

(defconst diaspora-regex-email
  "<\\(\\sw\\|\\s_\\|\\s.\\)+@\\(\\sw\\|\\s_\\|\\s.\\)+>"
  "Regular expression for matching inline email addresses.")

(defconst diaspora-regex-blockquote
  "^>.*$"
  "Regular expression for matching blockquote lines.")

(defconst diaspora-regex-hr
  "^\\(\\*[ ]?\\*[ ]?\\*[ ]?[\\* ]*\\|-[ ]?-[ ]?-[--- ]*\\)$"
  "Regular expression for matching markdown horizontal rules.")

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



(defvar diaspora-mode-font-lock-keywords
  (list
;   (cons diaspora-regex-bare-link '(2 diaspora-url-face t))
   (cons diaspora-regex-blockquote 'diaspora-blockquote-face)
   (cons diaspora-regex-user-entry 'diaspora-header-face-1)
   (cons diaspora-regex-header-1 'diaspora-header-face-1)
   (cons diaspora-regex-header-2 'diaspora-header-face-2)
   (cons diaspora-regex-header-3 'diaspora-header-face-3)
   (cons diaspora-regex-header-4 'diaspora-header-face-4)
   (cons diaspora-regex-hr 'diaspora-header-face-1)
   (cons diaspora-regex-image
         '((1 diaspora-link-face t)
           (2 diaspora-url-face t)))
   (cons diaspora-regex-bold '(2 diaspora-bold-face))
   (cons diaspora-regex-emph '(2 diaspora-emph-face))
   (cons diaspora-regex-code '(2 diaspora-inline-code-face))
   (cons diaspora-regex-email 'diaspora-link-face)
   (cons diaspora-regex-tag 'diaspora-url-face))
  "Syntax highlighting for diaspora files.")


;; Mode

(defun diaspora-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "diaspora.el, version %s" diaspora-el-version))

;;;###autoload
(define-derived-mode diaspora-mode text-mode "diaspora"
  "Major mode for output from \\[diaspora*]."
  (set (make-local-variable 'font-lock-defaults)
       '(diaspora-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t)
  (use-local-map diaspora-mode-map)
  (run-hooks 'diaspora-mode-hook))

(provide 'diaspora)

;;; diaspora.el ends here