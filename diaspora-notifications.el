;;; diaspora.el --- Simple Emacs-based client for diaspora*

;; Author: Christian Giménez
;; Maintainer: Tiago Charters de Azevedo <tca@diale.org>
;; Created: Jan 31, 2012
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

;; Save all the files in a DIR and add that DIR to `load-path'; 
;; for instance `(add-to-list 'load-path "~/emacs.el/disaspora.el/")' to your .emacs
;; Files: diaspora.el, diaspora-post.el  and diaspora-stream.el 


					; ********************
					; Constants
(defconst diaspora-notifications-buffer-name "*diaspora notifications*"
  "This is the name of the buffer that shows notifications from D*.")

					; ********************
					; Internal Variables
(defvar diaspora-notifications-url
  "notifications.json"
  "This is the URL for JSON format notifications.")

(defconst diaspora-notifications-read-all-url
  "notifications/read_all"
  "This is the URL used to make all notifications pass to the read state.")

					; ********************
					; Functions

(defun diaspora-add-key-to-w3m-link-keymap ()
  "Add to the `w3m-link-map' the keys necesary to use only the keyboard."
  (interactive)
  (define-key w3m-link-map "\C-c\C-o" 'diaspora-notification-goto-link)
  )

(defun diaspora-get-notifications () 
  "Get notifications from diáspora and show them in a new buffer"
  (interactive)
  (diaspora-ask)
  (diaspora-get-authenticity-token-if-necessary)
  (let ((http-buff (diaspora-get-url-entry-stream (diaspora-url diaspora-notifications-url)))
	(buff (get-buffer-create diaspora-notifications-buffer-name))
	(inhibit-read-only t))    
    (with-current-buffer http-buff
      (diaspora-delete-http-header))
    (diaspora-parse-notifications-json http-buff buff)
    (diaspora-kill-buffer-safe http-buff)

    (switch-to-buffer buff)
    (with-current-buffer buff
      (let ((inhibit-read-only t))
	(diaspora-mode)
	(diaspora-stream-mode)
	)      
      (setq buffer-read-only t)
      (goto-char (point-min)))))


(defun diaspora-parse-notifications-json (buffer buffer-to)
  "Parse all the notifications in the JSON that are in the given buffer, and put the result in the \"buffer-to\" buffer."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((lst-parsed (json-read))) ;;This returns an array of json notifications!
      (with-current-buffer buffer-to
	(delete-region (point-min) (point-max))
	(insert (diaspora-add-link-to-read-all "Mark all as read"))
	(let ((le (length lst-parsed)))	  
	  (dotimes (i le)
	    (diaspora-show-notification (aref lst-parsed i) buffer-to)))))))

(defun diaspora-show-notification (notification buffer)
  "Insert into buffer the JSON formated notification in a most human readable text."  
  (with-current-buffer buffer    
    (let ((type (car (car notification))))
      (diaspora-header-notifications notification buffer)
      (cond 
       ((eq type 'liked) (diaspora-liked-notification notification buffer))
       ((eq type 'started_sharing) (diaspora-started-sharing-notification notification buffer))
       ((eq type 'comment_on_post) (diaspora-comment-on-post-notification notification buffer))
       ((eq type 'mentioned) (diaspora-mentioned-notification notification buffer))
       ((eq type 'also_commented) (diaspora-comment-on-post-notification notification buffer))
       (t (diaspora-unknown-notifications notification buffer))))))

(defun diaspora-header-notifications (notification buffer-to)
  "Write the header of each notification. That is the common information of all types of notifications:
- Date
- Is unread?
"
  (with-current-buffer buffer-to
    (let ((date (cdr (assoc 'updated_at (cdr (car notification)))))
	  (unread (cdr (assoc 'unread (cdr (car notification)))))
	  (not-id (cdr (assoc 'id (cdr (car notification)))))
	  )      
      (insert "\n"
	      (propertize 
	       "          ====================          \n"
	       'diaspora-message-separator t)
	      (format  "At %s: " date))
      (if (or (eq unread :json-true)
	      (eq unread t))
	  (insert (propertize "Unread!"
			      'diaspora-is-unread-notification t)
		  "\n")
	(progn
	  (insert (propertize "Readed"
			      'diaspora-is-readed-notification t)
		 " | " )
	  (insert (diaspora-add-link-to-unread "Mark as Unread" not-id)
		  "\n")
	  )
	))))

(defun diaspora-unknown-notifications (notification buffer-to)
  "Write an unknown type of notification. That's mean, write every data in the notification."
  (with-current-buffer buffer-to
    (let ((date (cdr (assoc 'updated_at (cdr (car notification)))))
	  (unread (cdr (assoc 'unread (cdr (car notification)))))
	  (target (cdr (assoc 'target_type (cdr (car notification)))))
	  (recipient (cdr (assoc 'recipient_id (cdr (car notification)))))
	  (note (cdr (assoc 'note_html (cdr (car notification))))))
      (insert (format "\n<hr />\n%s:%s<br />" date note)))))

(defun diaspora-notification-remove-image-tags (line)
  "Remove the tags and replace it with the apropiate markdown."
  (replace-regexp-in-string "'>" ")" 
			    (replace-regexp-in-string "<img src='" "![Avatar](" line)))

(defun diaspora-notification-remove-link-tags (line)
  "Remove the link tags."
  (replace-regexp-in-string "</a>" ""
			    (replace-regexp-in-string "<a \[^>\]*>" "" line)))

(defun diaspora-mentioned-notification (notification buffer-to)
  "Write a \"mentioned\" notification."
  (with-current-buffer buffer-to
    (let ((target-id (cdr (assoc 'target_id (cdr (car notification)))))
	  (note-html (cdr (assoc 'note_html (cdr (car notification))))))
      (let ((splited-html (split-string note-html "\n")))
	(insert (diaspora-notification-remove-link-tags (nth 2 splited-html)) "\n")
	(when (string-match "/posts/\\([[:digit:]]*\\)" (nth 2 splited-html))
	  (insert (diaspora-add-link-to-publication "Goto publication" 
						    (string-to-number (match-string 1 (nth 2 splited-html)))) "\n"))))))

(defun diaspora-comment-on-post-notification (notification buffer-to)
  "Write a \"comment-on-post\" notification."
  (with-current-buffer buffer-to
    (let ((target-id (cdr (assoc 'target_id (cdr (car notification)))))
	  (note-html (cdr (assoc 'note_html (cdr (car notification)))))
	  )
      (let ((splited-html (split-string note-html "\n")))
	(insert 
	 (diaspora-notification-remove-image-tags (nth 1 splited-html))
	 "\n")
	
	(insert 
	 ;; Remove the name link property
	 (diaspora-notification-remove-link-tags (nth 2 splited-html))
	 "\n")
	(insert (diaspora-add-link-to-publication "Goto publication" target-id)
		"\n")
	 ))))

(defvar diaspora-notifications-mark-as-unread-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'diaspora-notifications-mark-as-unread-action)
    (define-key map [mouse-2] 'diaspora-notifications-mark-as-unread-action)
    map
    )
  "Keymap used for diaspora notifications \"mark as unread\" buttons."
  )

(defun diaspora-add-link-to-unread (text notification-id)
  "Return a text with button or link properties for mark as unread the post with id PUBLICATION-ID."
  (propertize text
	      'diaspora-notification-id  notification-id
	      'mouse-face 'diaspora-mouse-highlight-face
	      'help-echo "Click here to mark this publication as unread."
	      'keymap diaspora-notifications-mark-as-unread-map
	      'diaspora-is-notification-mark-as-unread t
	      )
  )

(defun diaspora-add-link-to-read-all (text)
  "Return a text with button or link properties for mark all notifications as readed."
  (propertize text
	      'mouse-face 'diaspora-mouse-highlight-face
	      'help-echo "Click here to mark all publications as readed."
	      'keymap diaspora-notifications-mark-all-as-read-map
	      'diaspora-is-notification-mark-as-unread t
	      )
  )

(defun diaspora-liked-notification (notification buffer-to)
  "Write a \"liked\" notification."
  (let ((target-id (cdr (assoc 'target_id (cdr (car notification)))))
	(note-html (cdr (assoc 'note_html (cdr (car notification))))))
    (let ((splited-html (split-string note-html "\n")))
      (insert (diaspora-notification-remove-image-tags (nth 1 splited-html)) "\n")
      (insert (diaspora-notification-remove-link-tags (nth 2 splited-html)) "\n")
      (insert (diaspora-add-link-to-publication "Goto publication" target-id) "\n"))))
   

(defvar diaspora-notifications-mark-all-as-read-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'diaspora-notifications-mark-all-as-read-action)
    (define-key map [mouse-2] 'diaspora-notifications-mark-all-as-read-action)
    map
    )
  "Keymap used for diaspora notifications \"mark as unread\" buttons."
  )

(defun diaspora-started-sharing-notification (notification buffer-to)
  "Write a \"started sharing\" notification. in buffer 'buffer-to'."
  (with-current-buffer buffer-to
    (let ((target-id (cdr (assoc 'target_id (cdr (car notification))))) ;;Target is a person here!
	  (note-html (cdr (assoc 'note_html (cdr (car notification))))))
      ;; Parse HTML!
      (let ((splited-html (split-string note-html "\n")))
	;; take off the <img src='... '> into ![Avatar](...).
	(insert 
	 (diaspora-notification-remove-image-tags (nth 1 splited-html))
	 "\n")
	
	(insert 
	 (diaspora-notification-remove-link-tags (nth 2 splited-html))
	 "\n")))))

(defun diaspora-notifications-mark-all-read ()
  "Mark all notifications as readed."
  (interactive)
  (diaspora-get-url (diaspora-notif-url "read_all"))
  )

(defun diaspora-notifications-mark-as-unread (notification-id)
  "Send a POST indicating that a notification must be marked as unread."
  (let ((url-request-method "PUT")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")
	   ("Accept-Language" . "en")	   
	   ("Accept-Charset" . "utf-8")))
	(buffer-file-coding-system 'utf-8)
	(url-request-data 
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		    (list 
		     (cons "user[username]" diaspora-username)
		     (cons "user[password]" diaspora-password)
		     (cons "user[remember_me]" "1")
		     (cons "authenticity_token" diaspora-auth-token)
		     (cons "set_unread" "true")
			  )
		    "&")))
    (push (cons "X-CSRF-Token" diaspora-auth-token) url-request-extra-headers)
    (diaspora-kill-buffer-safe 
     (url-retrieve-synchronously (diaspora-notif-url notification-id)))    
    )
  )

(defun diaspora-notifications-get-id-near-point ()
  "Return the nearest `diaspora-notification-id' property value."
  (get-text-property (+ 1 (previous-single-property-change (+ (point) 1) 'diaspora-notification-id))
		     'diaspora-notification-id)
  )
  

(defun diaspora-notifications-mark-as-unread-action (&rest)
  "Mark publication as unread."
  (interactive)
  (diaspora-notifications-mark-as-unread (diaspora-notifications-get-id-near-point))
  )

(defun diaspora-notifications-mark-all-as-read-action (&rest)
  "Mark publication as unread."
  (interactive)
  (diaspora-notifications-mark-all-read)
  )

(provide 'diaspora-notifications)