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


(defvar diaspora-send-type nil
  "This buffer shows what type of message is in the current buffer.
You should make it as a buffer-local variable using `make-local-variable' function and use it accordingly.

Expected values are:
* nil      :: It is not determined or unknown.
* 'post    :: Is a new post. 
* 'comment :: Is a new comment."
  )

(defun diaspora-set-send-type (type &optional buffer)
  "Set the type of buffer: 

* If it is a new post set TYPE to 'post.
* If it is a new comment set TYPE to 'comment"
  (unless buffer
    (setq buffer (current-buffer))
    )
  (with-current-buffer buffer
    (set (make-local-variable 'diaspora-send-type) type)
    )
  )

(defvar diaspora-post-edit-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'diaspora-send-post-or-comment-this-buffer)
    (define-key map "\C-c\C-h4" 'diaspora-markdown-insert-headline-4)
    (define-key map "\C-c\C-h3" 'diaspora-markdown-insert-headline-3)
    (define-key map "\C-c\C-h2" 'diaspora-markdown-insert-headline-2)
    (define-key map "\C-c\C-h1" 'diaspora-markdown-insert-headline-1)
    (define-key map "\C-c\C-u" 'diaspora-markdown-insert-unordered-list)
    (define-key map "\C-c\C-e" 'diaspora-markdown-insert-emph-text)
    (define-key map "\C-c\C-b" 'diaspora-markdown-insert-bold-text)
    (define-key map "\C-c\C-r" 'diaspora-markdown-insert-horizontal-rule)
    (define-key map "\C-c\C-l" 'diaspora-markdown-insert-link)
    (define-key map "\C-c\C-i" 'diaspora-markdown-insert-image)
    (define-key map "\C-c\C-m" 'diaspora-markdown-mention-user)
    (define-key map "\C-c\C-t" 'diaspora-markdown-insert-tag)
    map
    )
  "Keymap for `diaspora-post-edit-mode'."
  )

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

  ;; (if diaspora-post-edit-mode
  ;;     (diaspora-pem-add-keywords)
  ;;   (diaspora-pem-remove-keywords)
  ;;   )	    
  
  (unless diaspora-send-type
    (diaspora-set-send-type 'post)
    (message "Use C-c C-c to send this buffer as a new POST")
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

(defun diaspora-send-post-or-comment-this-buffer (&rest r)
  "Depending if this is a comment or post, send it.
Use `diaspora-post-this-buffer' for posting, or `diaspora-send-comment-this-buffer' if it is a new comment.

I read the `diaspora-send-type' variable and reset it to nil after sending."
  (interactive)
  (cond
   ((equal diaspora-send-type 'post)
    (diaspora-post-this-buffer)
    (message "Buffer POSTED!")
    )
   ((equal diaspora-send-type 'comment)
    (diaspora-send-comment-this-buffer)
    (message "COMMENT sended!")
    )
   (t 
    (message "D* cannot determine the type of buffer you are trying to send. Use C-cp for sending a new post or C-cc for sending a new comment."))
   )
  (set (make-local-variable 'diaspora-send-type) nil)
  )

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
  "@{" str " ; " _ (concat "@" diaspora-pod "}"))

(define-skeleton diaspora-markdown-insert-tag
  "Tag."
  "Tag(without #): "
  "#" str )

(provide 'diaspora-post-edit-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diaspora-edit-mode.el ends here
