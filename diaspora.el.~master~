;;; diaspora.el --- Simple Emacs-based client for diaspora*
;; Copyright 2011 Tiago Charters Azevedo
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

;(add-hook 'diaspora-status-edit-mode-hook 'longlines-mode)

(require 'url)
(require 'url-http)

(defvar diaspora-username nil)
(defvar diaspora-password nil)


(defconst diaspora-url-sign-in
  "https://joindiaspora.com/users/sign_in"
  "URL used to signing in.")

(defconst diaspora-url-status-messages
  "https://joindiaspora.com/status_messages"
  "URL used to update diaspora status messages.")

(defun diaspora-ask ()
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
		  
(provide 'diaspora)

