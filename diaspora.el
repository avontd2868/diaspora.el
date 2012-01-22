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
(require 'font-lock)

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

(defcustom diaspora-single-message-url
  "https://joindiaspora.com/posts"
  "URL used to get a single message.")

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

(defvar diaspora-temp-directory "~/.emacs.d/diaspora.el/"
  "Temporal directory where to save files for diaspora.el.")

(defvar diaspora-stream-buffer "*diaspora stream*"
  "The name of the diaspora stream buffer.")

(defvar diaspora-post-buffer "*diaspora post*"
  "The name of the diaspora post buffer.")

(defvar diaspora-single-message-buffer "*diaspora message*"
  "The name of the diaspora single message buffer."
  )

;;; User Functions:

;; (defun diaspora-create-file-post ()
;;   (interactive)
;;   (read-from-minibuffer "Find file: "
;; 			nil nil nil 'diaspora-post-file-name)
;;   (let ((post-buffer (get-buffer-create (car diaspora-post-file-name))))
;;     (switch-to-buffer post-buffer)
;;     (diaspora-mode)))

;; Posting

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
  "Ask for username and password if `diaspora-username' and  `diaspora-password' has not been setted."
  (unless (and
	   diaspora-username
	   diaspora-password)
      ;; Diaspora username and password was not setted.
    (list
     (setq diaspora-username (read-string "username: "
					  diaspora-username
					  nil nil))
     (setq diaspora-password (read-passwd "password: ")))))

(defun diaspora-authenticity-token (url)
  "Get the authenticity token."
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		    (list (cons "user[username]" diaspora-username)
			  (cons "user[password]" diaspora-password)
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
		    (list (cons "user[username]" diaspora-username)
			  (cons "user[password]" diaspora-password)
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
  (message (concat "Getting authenticity token..."))
  (message (concat "done: " diaspora-auth-token))
  (diaspora-authenticity-token diaspora-sign-in-url)
  (message (concat "done: " diaspora-auth-token))
  (diaspora-post (buffer-string))
  (diaspora-post-append-to-file)
  (kill-buffer))


					; *******************************
					; *** Getting the Main Stream ***


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

(defun diaspora-delete-http-header ()
  "Delete the first lines that is the HTTP header in the current buffer.
This is used after getting a stream or any URL in JSON format."
   (goto-char (point-min))
   (search-forward "\n\n")      
   (delete-region (point-min) (match-beginning 0))
  )

(defun diaspora-get-entry-stream ()
  "Show the entry stream. 
First look for the JSON file at `diaspora-entry-stream-url' and then parse it.
I expect to be already logged in. Use `diaspora' for log-in."
  (interactive)  
  (diaspora-ask) ;; don't forget username and password!
  (diaspora-authenticity-token diaspora-sign-in-url) ;; Get the authenticity token
  ;; get the in JSON format all the data
  (let ((buff (diaspora-get-url-entry-stream diaspora-entry-stream-url)))
    (with-current-buffer buff
      ;; Delete the HTTP header...
      (diaspora-delete-http-header)
      ;; Parse JSON...
      (diaspora-parse-json)

      ;;Change markdown to html... not so good.      
      ;;(diaspora-change-to-html)
      ;;Better using diaspora-mode already done by Tiago!      
      (diaspora-mode) 
      (set (make-local-variable 'buffer-read-only) t)

      (goto-char (point-min))
      )
    ;; Delete HTTP Buffer
    ;;(kill-buffer buff)
    ))


(defun diaspora-get-tmp-path (filename)
  "Return the path of temporal files. 
Check if the temporal directory exists, if not create it."
  (unless (file-exists-p diaspora-temp-directory)    
    (make-directory diaspora-temp-directory)
    )
  (format "%s/%s" diaspora-temp-directory filename)
  )

(defun diaspora-change-to-html ()
  "Change current buffer from markdown into html and htmlize"
  (write-file (diaspora-get-tmp-path "entry-stream.markdown"))
  (markdown-preview)
  )

(defvar diaspora-show-message-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'diaspora-show-message-new-buffer)
    (define-key map [mouse-2] 'diaspora-show-message-new-buffer)
    map)
  "Keymap used when the user clics on a name link.")

(defun diaspora-show-message (parsed-message &optional buffer)
  "Show a parsed message in a given buffer.
If buffer is nil, then use the `current-buffer'."
  ;; Ensure that buffer is not nil, in case is nil, buffer will be `current-buffer'.
  (let ((buffer (if (null buffer)
		    (current-buffer)
		  buffer)))
    (with-current-buffer buffer
      (let ( 
	    (id (cdr (assoc 'id parsed-message)))
	    (name (cdr (assoc 'name (assoc 'author parsed-message))))
	    (diaspora_id (cdr (assoc 'diaspora_id (assoc 'author parsed-message))))
	    (text (cdr (assoc 'text parsed-message)))
	    (date (cdr (assoc 'created_at parsed-message)))
	    (amount-comments (cdr (assoc 'comments_count parsed-message)))
	    (amount-likes (cdr (assoc 'likes_count parsed-message)))
	    ;; We can look for more data, including the last 3 comments!
	    )
	(insert  "---\n")
	(insert (propertize
		 (format "%s(%s):\n" name diaspora_id)
		 'mouse-face 'highlight
		 'face "link"
		 'keymap 'diaspora-show-message-map
		 'diaspora-id-message id
		 'help-echo "Click here to see this message in new buffer."))
	(insert (format "%s\n" date))
	(insert (format "Has %s comments. %s likes.\n" amount-comments amount-likes))
	(insert (format "%s\n\n" text))))))
  

(defun diaspora-show-message-new-buffer ()
  "Show this message in new buffer. Load the message, and all its comments, and show it!."
  (interactive)
  (let ((id-message (get-text-property (+ 1 
					  (previous-single-property-change (point) 'diaspora-id-message))
		     'diaspora-id-message)))
    
    (diaspora-get-single-message id-message)))

(defun diaspora-get-single-message (id-message)
  "Get from the `diaspora-single-message-url' URL the given message by id."
  (let ((buff (get-buffer-create diaspora-single-message-buffer))
	(buff-http (diaspora-get-url-entry-stream
		    (format "%s/%s.json" diaspora-single-message-url id-message))))
    
    (with-current-buffer buff-http
      ;; Delete HTTP header!
      (diaspora-delete-http-header)
      )
    
    (diaspora-parse-single-message-json buff-http buff)
    (switch-to-buffer buff)
    (diaspora-mode)))

(defun diaspora-parse-single-message-json (buff-from buff-to)
  "Parse JSON format of a single message from buffer \"buff-from\" and return into \"buff-to\""
  (with-current-buffer buff-from
    ;; Get the post message parsed from JSON
    (goto-char (point-min))
    (let ((lstparsed (cdr (assoc 'posts (json-read)))))
      (with-current-buffer buff-to
	;; Clean buffer buff-to and insert message
	(delete-region (point-min) (point-max))
	(diaspora-show-message lstparsed)
	)
      )    
    ))

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
  ;; Create a new buffer called according `diaspora-buffer' say and parse the json code into lists.
  (let ((lstparsed (cdr (assoc 'posts (json-read))))
	(buff (get-buffer-create diaspora-stream-buffer))) 
    ;; clean the new buffer
    (switch-to-buffer buff)
    (let ((le (length lstparsed))
	  (inhibit-read-only t))
      (delete-region (point-min) (point-max))
    ;; Show all elements
      (dotimes (i le)
	(diaspora-show-message (aref lstparsed i) buff)))))


(defsubst diaspora-date ()
  "Date string."
  (interactive)
  (insert "\n\n#" (format-time-string "%Y%m%d") " "))



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

(defcustom diaspora-regex-image
  "\\(!?\\[[^]]*?\\]\\)\\(([^\\)]*)\\)"
  "Regular expression for a [text](file) or an image link ![text](file)."
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regex-user-entry 
"^[a-zA-Z0-9_\s-]*([a-zA-Z0-9_\s-]*@[a-zA-Z0-9\s-]*\.[a-zA-Z0-9\s-]*)"
  "Regular expression for user entry."
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regex-tag
  "#\\([a-zA-Z0-9_]\\)+"
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