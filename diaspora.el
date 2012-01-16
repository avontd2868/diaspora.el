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


(require 'url)
(require 'url-http)
(require 'json)

(defgroup diaspora nil "diaspora* stream viewer"
  :group 'applications)

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

(defcustom diaspora-url-sign-in "https://joindiaspora.com/users/sign_in"
  "URL used to signing in."
  :group 'diaspora)

(defcustom diaspora-url-status-messages "https://joindiaspora.com/status_messages"
  "URL used to update diaspora status messages."
  :group 'diaspora)

(defcustom diaspora-entry-stream-url "https://joindiaspora.com/stream.json"
  "JSON version of the entry stream(the main stream)."
  :group 'diaspora)

(defun diaspora-ask ()
  "Ask for username and password."
  (list
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
			  (cons "user[password]" (car diaspora-password)))
		    "&")))
    (url-retrieve url 'diaspora-find-auth-token)))

(defun diaspora-find-auth-token (status)
  "Find the authenticity token."  
  (switch-to-buffer (current-buffer))
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
    (url-retrieve diaspora-url-status-messages
		  (lambda (arg) 
		    (kill-buffer (current-buffer))))))

(defun diaspora ()
  (interactive)
  (diaspora-ask)
  (diaspora-authenticity-token diaspora-url-sign-in)
  (diaspora-post (buffer-string)))



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

(defun diaspora-parse-json (&optional status)
  "Parse de JSON entry stream."
  (goto-char (point-min))
  (let ((lstparsed (cdr (assoc 'posts (json-read))))
	(buff (get-buffer-create "*diaspora*")))
    (switch-to-buffer buff)
    (let ((le (length lstparsed)))
    ;; Show all elements
      (dotimes (i le)
	(diaspora-show-message (aref lstparsed i) buff)))))		  

(provide 'diaspora)

