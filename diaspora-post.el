;;; diaspora.el --- Simple Emacs-based client for diaspora*

;; Author: Tiago Charters de Azevedo <tca@diale.org>
;; Maintainer: Tiago Charters de Azevedo <tca@diale.org>
;; Created: Jan 16, 2012
;; Version: .0
;; Keywords: diaspora*
;; URL: http://diale.org/diaspora.html

;; Copyright (c) 2012 Tiago Charters de Azevedo, Christian Giménez
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

(defun diaspora-post-to (&optional initial)
  "Post to diaspora.
With a prefix, uses the region as INITIAL.
For example: C-u M-x diaspora-post-to."
  (interactive
   (list (when current-prefix-arg
           (buffer-substring (point) (mark)))))
  (window-configuration-to-register diaspora-post-register)
  (get-buffer-create diaspora-post-buffer)
  (switch-to-buffer-other-window diaspora-post-buffer)
  (diaspora-date)
  (insert diaspora-footer-post)
  (goto-char (point-min))
  (when initial 
    (insert initial))
  (goto-char (point-min))
  (insert diaspora-header-post)
  (diaspora-mode)
  (message "Use C-cp to post to diaspora*."))

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
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "<meta name=\"csrf-token\" content=\"\\(.*\\)\"/>")
    (setq diaspora-auth-token (match-string-no-properties 1)))
  diaspora-auth-token)


(defun diaspora-post (post &optional id)
  "Post `post' to diaspora."
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
  (diaspora-save-post-to-file)
  (kill-buffer))

(defsubst diaspora-date ()
  "Date string."
  (interactive)
  (insert "\n\n#" (format-time-string "%Y%m%d") " "))

  
(defun diaspora-save-post-to-file ()
  (with-temp-buffer
    (insert-buffer diaspora-post-buffer)
    (insert "\n" "---" "\n")
    (let ((file-name-for-saving-post (format-time-string "%Y%m%d")))
      (if (find-buffer-visiting file-name-for-saving-post)
	  (let ((post-text (buffer-string)))
	    (set-buffer (get-file-buffer (concat diaspora-posts-directory file-name-for-saving-post)))
	    (save-excursion
	      (goto-char (point-max))
	      (insert post-text)
	      (insert "\n")
	      (when diaspora-save-after-posting (save-buffer))))
	(append-to-file (point-min) (point-max) 
			(concat diaspora-posts-directory file-name-for-saving-post))))))
	
(defun diaspora-find-all-markdown (regexp &optional opt)
  "Find all markdown strings given by `regexp' and return all of them in a list.
Usage example: `(diaspora-find-all-markdown diaspora-regex-tag)'"
  (flet ((d-find-aux (regexp)
		       (cond ((search-forward-regexp  regexp (point-max) t)
			      (cons (match-string-no-properties (if (not opt) 0 opt)) 
				    (d-find-aux regexp)))
			     (t nil))))
    (remove-duplicates (d-find-aux regexp) :test 'equal)))


(defun diaspora-post-buffer-desc ()
  "Using the first line of the current buffer."
    (interactive)
    (let ((post (buffer-substring (point-min)
				  (save-excursion
				    (goto-char (point-min))
				    (end-of-line)
				    (if (> (- (point) (point-min)) 60)
					(goto-char (+ (point-min) 60)))
				    (point)))))
      (diaspora-ask)
      (diaspora-post post)))

(defun diaspora-post-clipboard ()
  "Post to diaspora the contents of the current clipboard.
Most useful for take-notesing things from Netscape or other X Windows
application."
  (interactive)
  (diaspora-ask)
  (diaspora-post-to (current-kill 0)))

(defun diaspora-post-destroy ()
  "Destroy the current diaspora post buffer."
  (interactive)
  (when (equal diaspora-post-buffer (buffer-name))
    (kill-buffer (current-buffer))
    (jump-to-register diaspora-post-register)))


(defun diaspora-short-url (url)
  "Short URL function, user is.gd."
  (interactive "M")
  (let ((url-request-method "GET"))
    (url-retrieve (concat "http://is.gd/create.php?format=simple&url=" url)
                   (lambda (x)
		     (goto-char (point-min))
		     (search-forward-regexp "http://.*")
		     (setq s-url (match-string-no-properties 0))))
   (insert s-url)))

(provide 'diaspora-post)
