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
  (diaspora-authenticity-token diaspora-sign-in-url)
  (message (concat "done: " diaspora-auth-token))
  (diaspora-post (buffer-string))
  (diaspora-post-append-to-file)
  (kill-buffer))



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


(defun diaspora-find-all-user-in-buffer (&optional buffer)
  "Find all users in buffer BUFFER and return 
a list of strings `user name(username@joinsdiaspora.com)'"
;  (switch-to-buffer buffer)
  (cond ((search-forward-regexp  diaspora-regex-user-entry (point-max) t)
	 (cons (match-string-no-properties 0) (diaspora-find-all-user-in-buffer)))
	(t nil)))

(provide 'diaspora-post)