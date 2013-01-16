;; diaspora.el --- Simple Emacs-based client for diaspora*

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

(require 'cl)
(require 'markdown-mode)
(require 'diaspora-post-edit-mode)

(defcustom diaspora-header-post
  "### "
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
  (with-current-buffer diaspora-post-buffer
    (let ((inhibit-read-only t))
      (diaspora-date)
      (insert diaspora-footer-post)
      (goto-char (point-min))
      (when initial 
	(insert initial))
      (goto-char (point-min))
      (insert diaspora-header-post)
      (diaspora-mode)
      (diaspora-set-send-type 'post)
      (diaspora-post-edit-mode)      
      (set 'buffer-read-only nil)
      )
    )
  (message "Use C-cp to post to diaspora*."))

(defun diaspora-add-aspect (aspect-name)
  "Add an aspect to the list of aspects `diaspora-aspects-for-post' for posting.
This list is used as parameter for `diaspora-post'."
  (interactive 
   (let ((string (progn 
		   (diaspora-get-aspects)
		   (completing-read "Aspect name?" diaspora-aspect-alist)))
	 )
     (list string)))
  (let ((aspect-id (cdr (assoc aspect-name diaspora-aspect-alist))))
    (if (null aspect-id)
	(message "Aspect not founded.")
      (progn 
	(setq diaspora-aspects-for-post (push aspect-id diaspora-aspects-for-post))
	(message (concat "Aspect id Added: " 
			 (if (numberp aspect-id)
			     (number-to-string aspect-id)
			   aspect-id)))))))

(defun diaspora-clear-selected-aspects ()
  "Clear all the selected aspect to use with the next post."
  (interactive)
  (setq diaspora-aspects-for-post nil))

(defun diaspora-selected-aspects ()
  "Show the selected aspects to use with the newly post."
  (interactive)
 (let ((msg "Aspects: \n"))
    (dolist (i diaspora-aspects-for-post)
      (setq msg (concat msg 
			"-> "
			(car (rassoc i diaspora-aspect-alist)) 
			"\n")))
    (message msg)))

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
			  (cons "user[remember_me]" "1")
			  (cons "utf8" "✓"))
		    "&")))

    (diaspora-debug-msg "***POSTing:")
    (diaspora-debug-msg url)
    (diaspora-debug-msg url-request-data)

    (with-current-buffer (url-retrieve-synchronously url)
      (diaspora-find-auth-token)
      (diaspora-kill-buffer-safe)))
  )

(defun diaspora-find-auth-token (&optional status)
  "Find the authenticity token."  
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward-regexp "<meta name=\"csrf-token\" content=\"\\(.*\\)\"/>" nil t)
      (search-forward-regexp "<meta content=\"\\(.*\\)\" name=\"csrf-token\"[[:blank:]]*/>" nil t))
    (setq diaspora-auth-token (match-string-no-properties 1)))
  diaspora-auth-token)

(defun diaspora-aspect-post-parameter (aspects_ids)
  "Concat the parameters in a string with commas. This is usefull to pass
as parameters for a POST.

If aspects_ids is nil, I return the string \"public\".

It doesn't matter if aspects_id has a string or number values as elements(or mixed!) it will concat it as well. "
  (if (null aspects_ids)
      "public"    
    (let ((salida ""))
      (dolist (i aspects_ids)
	(setq salida (concat salida 
			     (if (numberp i)
				 (number-to-string i)
			       i)
			     ",")))  
      (substring salida 0 -1))))

(defun diaspora-post-last-post-text ()
  (interactive)
  (diaspora-post diaspora-last-post-text))

(defun diaspora-image-list (lst-photos-ids)
  "Return a list with cons where the first element is the string \"photos[n]\" changing n accordingly, and the second element is the photo id taken from LST-PHOTOS-IDS."
  (let ((num 0)
	(outlst nil)
	)
    (dolist (e lst-photos-ids)
      (push (cons "photos[]" (number-to-string e)) outlst)
      (setq num (+ 1 num))
      )
    outlst
    )
  )

(defun diaspora-post (post &optional aspects_ids photos-ids)
  "Post POST to diaspora.
ASPECTS_IDS is a list of strings or numbers of aspects ids.
PHOTOS-IDS is a list of strings or numbers of photos ids."
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		    (append 
		     (list (cons "user[username]" diaspora-username)
			   (cons "user[password]" diaspora-password)
			   (cons "status_message[text]" post)
			   (cons "status_message[provider_display_name]" "diaspora.el") ;; Founded in Tiago Charters Azevedo's Diaspora-el
			   (cons "user[remember_me]" "1")
			   (cons "authenticity_token" diaspora-auth-token)
			   (cons "commit" "Sign in")
			   (cons "aspect_ids[]" (diaspora-aspect-post-parameter aspects_ids)))
		     (diaspora-image-list photos-ids))
		    "&"))
	)

    (diaspora-debug-msg "***POSTing:")
    (diaspora-debug-msg (diaspora-url diaspora-status-messages-url))
    (diaspora-debug-msg url-request-data)


    (url-retrieve (diaspora-url diaspora-status-messages-url)
		  (lambda (arg) (diaspora-kill-buffer-safe)))
    )		    
  )

  
		  

(defun diaspora-post-this-buffer ()
  "Post the current buffer to diaspora."
  (interactive)
  (diaspora-ask)
  (diaspora-get-authenticity-token-if-necessary)
  (diaspora-post (buffer-string) diaspora-aspects-for-post diaspora-images-posted)
  (setq diaspora-images-posted nil)
  (diaspora-save-post-to-file)
  (diaspora-kill-buffer-safe)
  )

(defsubst diaspora-date ()
  "Date string for inserting in posts."
  (interactive)
  (insert "\n\n#" (format-time-string "%Y%m%d") " "))

  
(defun diaspora-save-post-to-file ()
  "Save post to backup file. Backup file is ymd, a new post is append."
  (with-temp-buffer
    (insert-buffer diaspora-post-buffer)
    (setq diaspora-last-post-text (buffer-string))  ;this is temporary...
    (insert "\n" "---" "\n")
    (let ((file-name-for-saving-post (format-time-string "%y%m%d")))
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

(defun diaspora-find-all-regexp (regexp &optional num)
  "Try to find all the texts with match the given regexp.
Return all of them in a list.
NUM is the parenthesized expression in the regexp, like parameter NUM in `match-string-no-properties'."
  (cond ((search-forward-regexp  regexp (point-max) t)
	 (cons (match-string-no-properties (if (not num) 0 num))
	       (search-forward-regexp regexp))
	 )	 
	
	(t 
	 nil)
	)
  )

	
(defun diaspora-find-all-markdown (regexp &optional opt)
  "Find all markdown strings given by REGEXP and return all of them in a list.
Usage example: `(diaspora-find-all-markdown diaspora-regex-tag)'"
  (remove-duplicates (diaspora-find-all-regexp regexp opt) :test 'equal)
  )


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
Most useful for posting things from any where."
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
  "Short URL function, uses is.gd."
  (interactive "M")
  (let ((url-request-method "GET"))
    (url-retrieve (concat "http://is.gd/create.php?format=simple&url=" url)
		  (lambda (x)
		    (goto-char (point-min))
		    (search-forward-regexp "http://.*")
		    (setq s-url (match-string-no-properties 0))))
    (insert s-url)))

(defun diaspora-post-send-image (image-path url)
  "Send an image file given by IMAGE-PATH to the given URL."
  (with-current-buffer (find-file-literally image-path)
    (let ((image-data nil)
	  (url-request-method "POST")
	  (url-request-extra-headers
	   (list (cons "Content-Type" "application/octet-stream")
		 (cons "X-File-Name" (file-name-nondirectory image-path))
		 (cons "X-CSRF-Token" diaspora-auth-token)
		 (cons "Referer" "https://joindiaspora.com/stream")
		 )	   
	   )
	  (url-request-data (buffer-string))	  
	  )

      (diaspora-debug-msg "***POSTing:")
      (diaspora-debug-msg url)
      (diaspora-debug-msg url-request-data)

      (with-current-buffer (url-retrieve-synchronously url)
	(diaspora-delete-http-header)
	(goto-char (point-min))
	(setq image-data (json-read)) ;; save image url and data for history
	(diaspora-kill-buffer-safe)
	)      
      (diaspora-save-image-data image-data)
      (push (cdr (assoc 'id (diaspora-image-data-get-photo-data image-data)))
	    diaspora-images-posted)

      )
    )
  )

(defvar diaspora-images-posted
  nil
  "This is a list of images ids that has been posted.

This variable should store all the images ids of those unpublished images(temporary submitted).

It will be erased when you use `diaspora-post-this-buffer' or simmilar functions that post the message with the images ids."
  )

(defun diaspora-save-image-data (image-data)
  "Save the image data in a history file `diaspora-image-history-file'."
  (let* ((photo-data (diaspora-image-data-get-photo-data image-data))
	 (photo-urls (assoc 'unprocessed_image photo-data))
	)
    (with-temp-buffer 
      (if (file-exists-p diaspora-image-history-file)
	  (insert-file-contents diaspora-image-history-file)
	)
      (insert (format "\n* Photo
%s
** URL
%s
** Scaled full:
%s"
		      (cdr (assoc 'id photo-data))	      
		      (cdr (assoc 'url photo-urls))		      
		      (cdr (assoc 'url (cdr (assoc 'scaled_full photo-urls))))
		      )
	      )
      (write-file diaspora-image-history-file nil)
      )
    )
  )

(defun diaspora-image-data-get-photo-data (image-data)
  (cdr (assoc 'photo (cdr (assoc 'data image-data))))
  )

(defun diaspora-add-image (image-path)
  "Add an image to the next post."
  (interactive "fImage file?")
  (diaspora-ask)
  (diaspora-get-authenticity-token-if-necessary)
  (unless diaspora-aspects-for-post
    (diaspora-get-aspects)
    ;; (setq diaspora-aspects-for-post (diaspora-get-values diaspora-aspect-alist))
    ;; (setq diaspora-aspects-for-post (remove "all_aspects"
    ;; 					    (remove "public" diaspora-aspects-for-post)))
    (setq diaspora-aspects-for-post '("public"))
    )
  (diaspora-post-send-image image-path 
			    (diaspora-image-url t
						diaspora-aspects-for-post 
						(file-name-nondirectory image-path)
						)
			    )
  )
  

(provide 'diaspora-post)
