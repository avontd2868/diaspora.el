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



(require 'url)
(require 'url-http)
(require 'json)

(defconst diaspora-el-version ".0"
  "This version of diaspora*-el.")

(defgroup diaspora nil 
  "A mode for diaspora* stream view and posting."
  :group 'applications)

;;; User variable:

(defcustom diaspora-mode-hook nil
  "Functions run upon entering `diaspora-mode'."
  :type 'hook
  :options '(flyspell-mode turn-on-auto-fill)
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

(defun diaspora-post-to ()
  (interactive)
  (get-buffer-create diaspora-post-buffer)
  (switch-to-buffer diaspora-post-buffer)
  (diaspora-date)
  (insert diaspora-footer-post)
  (goto-char (point-min))
  (insert diaspora-header-post)
  (diaspora-mode))

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

(defun diaspora-authenticity-token (url)
  "Get the authenticity token."
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		    (list (cons "user[username]" (car diaspora-username))
			  (cons "user[password]" (car diaspora-password))
			  (cons "user[remember_me]" "1"))
		    "&")))
    (url-retrieve url 'diaspora-find-auth-token)))

(defun diaspora-find-auth-token (&optional status)
  "Find the authenticity token."  
;  (switch-to-buffer (current-buffer))
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "<meta name=\"csrf-token\" content=\"\\(.*\\)\"/>")
    (setq diaspora-auth-token (match-string-no-properties 1)))
  diaspora-auth-token)


(defun diaspora-post (post &optional id)
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		    (list (cons "user[username]" (car diaspora-username))
			  (cons "user[password]" (car diaspora-password))
			  (cons "status_message[text]" post)
			  (cons "user[remember_me]" "1")
			  (cons "authenticity_token" diaspora-auth-token)
			  (cons "commit" "Sign in")
			  (cons "aspect_ids[]" "public"))
		    "&")))
    (url-retrieve diaspora-status-messages-url
		  (lambda (arg) 
		    (kill-buffer (current-buffer))))))

(defun diaspora-post-this-buffer ()
  (interactive)
  (diaspora-ask)
  (diaspora-authenticity-token diaspora-sign-in-url)
  (diaspora-post (buffer-string))
  (diaspora-post-append-to-file)
  (kill-buffer))

(defun diaspora-show-stream (status &optional new-buffer-name)
  "Show what was recieved in a new buffer.
If new-buffer-name is given then, the new buffer will have that name, 
if not, the buffer called \"Diáspora Stream\" will be re-used or created if needed."
  ;; new-buffer-name has been given? if not, use `diaspora-stream-buffer´ as name.
  (unless new-buffer-name
    (setq new-buffer-name diaspora-stream-buffer))
  (let ((buffer (get-buffer-create new-buffer-name))
	(text (buffer-string))
	(buf-kill (current-buffer)))    
    ;; copy text and switch
    (switch-to-buffer buffer)
    (insert text)    
    ;; kill the http buffer
    (kill-buffer buf-kill)))

(defun diaspora-get-url-entry-stream (url)
  "Get the Diáspora URL and leave it in a new buffer."
  (let ((url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")
	   ("Accept-Language" . "en")
	   ("Accept-Charset" . "UTF-8"))))
    (url-retrieve-synchronously url)))


(defun diaspora-get-entry-stream ()
  "Show the entry stream. 
First look for the JSON file at `diaspora-entry-stream-url' and then parse it.
I expect to be already logged in. Use `diaspora' for log-in."
  (interactive)  
  (diaspora-ask) 
  (diaspora-authenticity-token diaspora-sign-in-url) ;; Get the authenticity token
  (let ((buff (diaspora-get-url-entry-stream diaspora-entry-stream-url)))
    (with-current-buffer buff
      ;; Delete the HTTP header...
      (goto-char (point-min))
      (search-forward "\n\n")      
      (delete-region (point-min) (match-beginning 0))
      ;; Parse JSON...
      (diaspora-parse-json))
    ;; Delete HTTP Buffer
    (kill-buffer buff)))

(defun diaspora-show-message (parsed-message &optional buffer)
  "Show a parsed message in a given buffer."
  (with-current-buffer buffer
    (let ( (name (cdr (assoc 'name (assoc 'author parsed-message))))
	  (diaspora_id (cdr (assoc 'diaspora_id (assoc 'author parsed-message))))
	  (text (cdr (assoc 'text parsed-message)))
	  (date (cdr (assoc 'created_at parsed-message)))
	  (amount-comments (cdr (assoc 'comments_count parsed-message)))
	  (amount-likes (cdr (assoc 'likes_count parsed-message))))
      (insert (format "---\n%s(%s):\n%s\n\n" name diaspora_id text)))))

(defun diaspora-get-entry-stream-tag (tag)
  "Get stream of tag. Just an idea... needs working."
  (let ((buff (diaspora-get-url-entry-stream
	       (concat "https://joindiaspora.com/tags/" tag ".json"))))
    (with-current-buffer buff
      (goto-char (point-min))
      (search-forward "\n\n")      
      (delete-region (point-min) (match-beginning 0))
      (diaspora-parse-json))
    (kill-buffer buff)))

(defun diaspora-parse-json (&optional status)
  "Parse de JSON entry stream."
  (goto-char (point-min))
  (let ((lstparsed (cdr (assoc 'posts (json-read))))
	(buff (get-buffer-create diaspora-stream-buffer)))
    (switch-to-buffer buff)
    (let ((le (length lstparsed)))
    ;; Show all elements
      (dotimes (i le)
	(diaspora-show-message (aref lstparsed i) buff)))))


(defsubst diaspora-date ()
  "Date string."
  (interactive)
  (insert "\n\n#" (format-time-string "%Y%m%d") "\n"))



(defun diaspora-post-append-to-file ()
  ;; Based on take-notes.el/remember.el
  (with-temp-buffer
    (insert-buffer diaspora-post-buffer)
    (insert "\n" "---" "\n")
    (if (find-buffer-visiting diaspora-data-file)
	(let ((post-text (buffer-string)))
	  (set-buffer (get-file-buffer diaspora-data-file))
	  (save-excursion
	    (goto-char (point-min))
	    (insert post-text)
	    (insert "\n")
	    (when diaspora-save-after-posting (save-buffer)))
	  (append-to-file (point-min) (point-max) diaspora-data-file)))))

(defvar diaspora-mode-map 
"Keymap based on html-mode"
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
    (define-key diaspora-mode-map "\C-cp" 'diaspora-post-this-buffer)
    diaspora-mode-map)
    "Keymap used in diaspora-mode.")

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

(defun diaspora-mode ()
  "Major mode for output from \\[diaspora*]."
  (interactive)
  (kill-all-local-variables)
  (indented-text-mode)
  (use-local-map diaspora-mode-map)
  (setq major-mode 'diaspora-mode
        mode-name "diaspora")
  (run-hooks 'diaspora-mode-hook))


;; regexp for  highlight; similar to emacs-muse
;; http://www.gnu.org/software/emacs-muse/

(defcustom diaspora-explicit-link-regexp
  "\\[\\([^][\n]+\\)\\]\\(\\([^][\n]+\\)\\)"
  "Regexp used to match [Text](URL) links.
Paren group 1 must match the URL, and paren group 2 the description."
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-explicit-image-regexp
  "!\\[\\([^][\n]+\\)\\]\\(\\([^][\n]+\\)\\)"
  "Regexp used to match [Text](ImageURL)]] links.
Paren group 1 must match the URL, and paren group 2 the description."
  :type 'regexp
  :group 'diaspora)


(provide 'diaspora)

;;; diaspora.el ends here