;;; diaspora.el --- Simple Emacs-based client for diaspora*

;; Author:  Tiago Charters Azevedo, Christian Giménez
;; Keywords: diaspora*

;; Copyright 2011 Tiago Charters Azevedo, Christian Giménez
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

(provide 'diaspora)

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
  :options '(flyspell-mode turn-on-auto-fill longlines-mode)
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

(defcustom diaspora-header-post
  ""
  "Header for each post:")

(defcustom diaspora-footer-post
  "#diaspora-el"
  "Footer for each post.")

;;; Internal Variables:

(defvar diaspora-buffer "*diaspora*"
  "The name of the diaspora stream buffer.")

;;; User Functions:

(defun diaspora-create-file-post ()
  (interactive)
  (read-from-minibuffer "Find file: "
			nil nil nil 'diaspora-post-file-name)
  (let ((post-buffer (get-buffer-create (car diaspora-post-file-name))))
    (switch-to-buffer post-buffer)
    (diaspora-mode)))

(defun diaspora-post-to ()
  (interactive)
  (let* ((name-file (format-time-string "%y%m%d%H%M%s"))
	(post-buffer (get-buffer-create name-file)))
    (switch-to-buffer post-buffer)
    (insert diaspora-header-post)
    (diaspora-date)
    (insert diaspora-footer-post)
    (goto-char (point-min))
    (diaspora-mode)
    (write-file (concat diaspora-entry-file-dir name-file))))

(defun diaspora-ask ()
  "Ask for username and password if `diaspora-username' and  `diaspora-password' has not been setted."
  (unless (and
	   diaspora-username
	   diaspora-password)
      ;; Diaspora username and password was not setted.
    (list
     (read-from-minibuffer "username: "
			   (car diaspora-username)
			   nil nil
			   'diaspora-username)
     (setq diaspora-password (read-passwd "password: ")))))

   
(defun diaspora-authenticity-token (url)
  "Get the authenticity token."
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		    (list (cons "user[username]" (car diaspora-username))
			  (cons "user[password]" diaspora-password))
		    "&")))    
    (with-current-buffer (url-retrieve-synchronously url)
      (diaspora-find-auth-token))))

(defun diaspora-find-auth-token (&optional status)
  "Find the authenticity token."  
;  (switch-to-buffer (current-buffer))
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "<meta name=\"csrf-token\" content=\"\\(.*\\)\"/>")
    (setq auth-token (match-string-no-properties 1)))
  auth-token)


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
			  (cons "authenticity_token" auth-token)
			  (cons "commit" "Sign in")
			  (cons "aspect_ids[]" "public"))
		    "&")))
    (url-retrieve diaspora-status-messages-url
		  (lambda (arg) 
		    (kill-buffer (current-buffer))))))

(defun diaspora-post-buffer ()
  (interactive)
  (diaspora-ask)
  (diaspora-authenticity-token diaspora-sign-in-url)
  (diaspora-post (buffer-string))
  (kill-buffer))


					; *******************************
					; *** Getting the Main Stream ***


(defun diaspora-show-stream (status &optional new-buffer-name)
  "Show what was recieved in a new buffer.
If new-buffer-name is given then, the new buffer will have that name, 
if not, the buffer called \"Diáspora Stream\" will be re-used or created if needed."
  ;; new-buffer-name has been given? if not, use "Diáspora Stream" as name.
  (unless new-buffer-name
    (setq new-buffer-name "**Diaspora Stream**"))
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
  (diaspora-ask) ;; don't forget username and password!
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
	  (amount-likes (cdr (assoc 'likes_count parsed-message)))
	  ;; We can look for more data, including the last 3 comments!
	  )
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
	(buff (get-buffer-create diaspora-buffer)))
    (switch-to-buffer buff)
    (let ((le (length lstparsed)))
    ;; Show all elements
      (dotimes (i le)
	(diaspora-show-message (aref lstparsed i) buff)))))


(defsubst diaspora-date ()
  "Date string."
  (interactive)
  (insert "\n\n#" (format-time-string "%Y%m%d") "\n"))



;;; Internal Functions:

(defvar diaspora-mode-map ()
  "Keymap used in diaspora-mode.")
(when (not diaspora-mode-map)
  (setq diaspora-mode-map (make-sparse-keymap))
  (define-key diaspora-mode-map "\C-c\C-c" 'diaspora-post-buffer))

(defun diaspora-mode ()
  "Major mode for output from \\[diaspora*]."
  (interactive)
  (kill-all-local-variables)
  (indented-text-mode)
  (use-local-map diaspora-mode-map)
  (setq major-mode 'diaspora-mode
        mode-name "diaspora")
  (run-hooks 'diaspora-mode-hook))

;;; diaspora.el ends here
