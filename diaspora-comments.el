;;; diaspora-comments.el --- 
;; 
;; Filename: diaspora-comments.el
;; Description: 
;; Author: Christian Giménez, Tiago Charters Azevedo
;; Maintainer: 
;; Created: mar feb 14 13:15:51 2012 (-0300)
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
;; Functions for manage comments for diáspora.el.
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

					; ********************
					; Customization

(defcustom diaspora-comment-name 
  "comments"
  "This is the name of the comments for posting."
  :type 'string
  :group 'diaspora-streams)


					; ********************
					; Functions

(defun diaspora-insert-comments-for-message (message-id &optional buffer)
  "Get the comments for the given message, and insert it in the current 
buffer or in the buffer specified."
  (let ((buff-http (diaspora-get-url-entry-stream 
		    (diaspora-get-comment-url message-id)))
	(buffer (if (null buffer)
		    (current-buffer)
		  buffer)))
    (with-current-buffer buff-http
      (diaspora-delete-http-header)
      (let ((json-array-type 'list)
	    (json-object-type 'alist)
	    (lstparsed (json-read)))
	;; parse all comments one by one and insert it
	(let ((le (length lstparsed))
;	      (inhibit-read-only t)
	      )
	  (dotimes (i le)
	    (diaspora-insert-comment (aref lstparsed i) buffer)))))))
	    
(defun diaspora-insert-comment (comment buffer)
  "Insert a JSON parsed (with `json-read') into a specific buffer."
  (let ((name (cdr (assoc 'name (cdr (assoc 'author comment)))))
	(text (cdr (assoc 'text comment)))
	(created_at (cdr (assoc 'created_at comment))))
    (with-current-buffer buffer
      (insert (format "\n---\n%s at %s:\n" name created_at))
      (insert text))))


(defconst diaspora-comment-buffer-name "*diaspora comment*"
  "This is the name of the comment buffer.")

(defvar diaspora-comment-buffer nil
  "This is the buffer (supposed to be only one or unique) for write a comment.")

(defun diaspora-comment-message ()
  "Find the post-id and create a buffer for the user so he can write a comment.

This function set the `diaspora-next-comment-to-post' variable with the post-id."
  (interactive)
  (let ((post-id (diaspora-get-id-message-near-point)))
    (if post-id
	(diaspora-new-comment-buffer post-id)
      (message "Unable to find the post-id (located in the text property `diaspora-id-message').")
      )
    )
  )

(defun diaspora-new-comment-buffer (post-id)
  "Create a new buffer for write a comment for the post with id given by post-id."
  (set 'diaspora-next-comment-to-post post-id)
  ;; create buffer
  (set 'diaspora-comment-buffer (get-buffer-create diaspora-comment-buffer-name))
  (switch-to-buffer-other-window diaspora-comment-buffer)
  (with-current-buffer diaspora-comment-buffer
    (let ((inhibit-read-only t))
      ;; insert header and footer, modes... etc.
      (diaspora-date)
      (insert diaspora-footer-post)
      (goto-char (point-min))
      (insert diaspora-header-post)
      (diaspora-mode)
      (set 'buffer-read-only nil)
    ))
  (message "Use C-c C-c to comment to diaspora or use diaspora-send-comment-this-buffer."))

(defvar diaspora-next-comment-to-post
  nil
  "This is the post id where to send the comment in the next `diaspora-send-comment-this-buffer' function call.")

(defun diaspora-send-comment-this-buffer ()
  "Send this buffer as a comment to the post determined by the id `diaspora-next-comment-to-post'."
  (interactive)
  (diaspora-ask)
  (when (null diaspora-auth-token)
    (message (concat "Getting authenticity token..."))
    (diaspora-authenticity-token (diaspora-url diaspora-sign-in-url))
    (message (concat "done: " diaspora-auth-token))
    )     
  (diaspora-send-comment-post (buffer-string) diaspora-next-comment-to-post))


(defun diaspora-send-comment-post (comment post-id)
  "Send a comment for the post given by the post-id.
Comment should be a String and post-id the id number of the post."
  (let ((url-request-method "POST") 
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		    (list (cons "user[username]" diaspora-username)
			  (cons "user[password]" diaspora-password)
			  (cons "text" comment)
			  (cons "user[remember_me]" "1")
			  (cons "authenticity_token" diaspora-auth-token)
			  (cons "commit" "Sign in"))
		    "&")))
    (url-retrieve-synchronously (diaspora-post-comment-url post-id))))

;; Add keymap for `diaspora-mode':

(provide 'diaspora-comments)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diaspora-comments.el ends here
