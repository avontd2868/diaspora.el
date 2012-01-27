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

;, Streaming 

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
   (delete-region (point-min) (match-beginning 0)))

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
;      (set (make-local-variable 'buffer-read-only) t)
      (goto-char (point-min)))
    ;; Delete HTTP Buffer
    ;;(kill-buffer buff)
    ))

(defun diaspora-get-temp-path (filename)
  "Return the path of temporal files. 
Check if the temporal directory exists, if not create it."
  (unless (file-exists-p diaspora-temp-directory)    
    (make-directory diaspora-temp-directory))
  (format "%s/%s" diaspora-temp-directory filename))

(defun diaspora-change-to-html ()
  "Change current buffer from markdown into html and htmlize"
  (write-file (diaspora-get-temp-path "entry-stream.markdown"))
  (markdown-preview))

(defvar diaspora-show-message-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'diaspora-show-message-new-buffer)
    (define-key map [mouse-2] 'diaspora-show-message-new-buffer)
    map)
  "Keymap used when the user clics on a name link.")

(defvar diaspora-stream-message-map
  (let ((map (make-sparse-keymap)))
    (define-key map [\C-q] 'diaspora-single-message-destroy)
    map)
  "Keymap used in the stream and messages buffers.")


(defun diaspora-show-message (parsed-message &optional buffer)
  "Show a parsed message in a given buffer.
If buffer is nil, then use the `current-buffer'."
  ;; Ensure that buffer is not nil, in case is nil, buffer will be `current-buffer'.
;  (princ parsed-message)
  (let ((buffer (if (null buffer)
		    (current-buffer)
		  buffer)))
    (with-current-buffer buffer
      (let ((id (cdr (assoc 'id parsed-message)))
	    (name (cdr (assoc 'name (assoc 'author parsed-message))))
	    (diaspora_id (cdr (assoc 'diaspora_id (assoc 'author parsed-message))))
	    (text (cdr (assoc 'text parsed-message)))
	    (date (cdr (assoc 'created_at parsed-message)))
	    (avatar (cdr (assoc 'small (assoc 'avatar (assoc 'author parsed-message)))))
	    (amount-comments (cdr (assoc 'comments_count parsed-message)))
	    (amount-likes (cdr (assoc 'likes_count parsed-message)))
	    ;; We can look for more data, including the last 3 comments!
	    )
	(insert  "---\n")
	(insert "![" name "](" avatar ")\n")
	(insert (propertize
		 (format "%s(%s):\n" name diaspora_id)
		 'mouse-face 'highlight
		 'face "link"
		 'keymap diaspora-show-message-map
		 'diaspora-id-message id
		 'help-echo "Click here to see this message in new buffer."))
	(insert (format "%s\n" date))
	(insert (format "Has %s comments. %s likes.\n" amount-comments amount-likes))
	(insert (format "%s\n\n" text))))))
  

(defun diaspora-show-message-new-buffer (&rest r)
  "Show this message in new buffer. Load the message, and all its comments, and show it!."
  (interactive)
  (let ((id-message 
	 (get-text-property (+ 1 (previous-single-property-change (point) 'diaspora-id-message))
			    'diaspora-id-message)))
    (diaspora-get-single-message id-message))
;This does not work!      (set (make-local-variable 'buffer-read-only) t))
  )

(defun diaspora-single-message-destroy ()
  "Destroy the current diaspora single message buffer."
  (interactive)
  (when (equal diaspora-single-message-buffer (buffer-name))
    (kill-buffer (current-buffer))
    (jump-to-register diaspora-single-message-register)))


(defun diaspora-get-single-message (id-message)
  "Get from the `diaspora-single-message-url' URL the given message by id."
  (window-configuration-to-register diaspora-single-message-register)
  (let ((buff (get-buffer-create diaspora-single-message-buffer))
	(buff-http (diaspora-get-url-entry-stream
		    (format "%s/%s.json" diaspora-single-message-url id-message))))
    (with-current-buffer buff-http
      ;; Delete HTTP header!
      (diaspora-delete-http-header))
    (diaspora-parse-single-message-json buff-http buff)
    (diaspora-insert-comments-for-message id-message buff)
    (switch-to-buffer-other-window buff)
;    (switch-to-buffer buff)
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
	(diaspora-show-message lstparsed)))))

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
  ;; Create a new buffer called according `diaspora-buffer' say 
  ;; and parse the json code into lists.
  (let ((lstparsed (cdr (assoc 'posts (json-read))))
	(buff (get-buffer-create diaspora-stream-buffer))) 
    ;; clean the new buffer
    (switch-to-buffer buff)
    (let ((le (length lstparsed)))
	  ;(inhibit-read-only t))
      (delete-region (point-min) (point-max))
    ;; Show all elements
      (dotimes (i le)
	(diaspora-show-message (aref lstparsed i) buff)))))

(defun diaspora-insert-comments-for-message (message-id &optional buffer)
  "Get the comments for the given message, and insert it in the current 
buffer or in the buffer specified."
  (let ((buff-http (diaspora-get-url-entry-stream 
		    (format "%s/%s/comments.json" diaspora-single-message-url message-id)))
	(buffer (if (null buffer)
		    (current-buffer)
		  buffer)))
    (with-current-buffer buff-http
      (diaspora-delete-http-header)
      (let ((lstparsed (json-read)))
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

;; images: needs working

(defun diaspora-get-user-avatar (url &optional user-id)
  (let ((url-request-method "GET")
	(url-show-status nil))
	(url-retrieve url 'diaspora-write-image
		      (list url user-id))))
				 
(defun diaspora-get-image (url)
  (let ((url-request-method "GET")
	(url-show-status nil))
	(url-retrieve url 'diaspora-write-image
		      (list url))))

(defun diaspora-write-image (status url &optional user-id)
  (let ((image-file-name
	 (concat diaspora-user-image-dir "/" 
		 (if user-id 
		     (concat user-id "-"))
		 (file-name-nondirectory url))))
    (when (not (file-directory-p diaspora-user-image-dir))
      (make-directory diaspora-user-image-dir))
    (setq buffer-file-coding-system 'no-conversion)
    (setq buffer-file-name image-file-name)
    (goto-char (point-min))
    (delete-region (point-min) (search-forward "\C-j\C-j"))
    (save-buffer 0)
    (kill-buffer (current-buffer))))

(defun diaspora-get-all-images ()
  (interactive)
  (mapcar 'diaspora-get-image (diaspora-get-all-image-links)))

(defun diaspora-show-images (&optional opt)
  "If OPT nil shows images."
  (interactive)
  (goto-char (point-min))
  (let ((images-points (diaspora-get-all-regexp-markdown-points diaspora-regexp-image)))
    (save-excursion
      (dolist (ipoint images-points)
	(if (not opt)
	    (add-text-properties (cadr ipoint) (cddr ipoint)
				 (list 'display (create-image 
						 (concat diaspora-user-image-dir "/" 
							 (file-name-nondirectory 
							  (car ipoint))))))
	  (remove-text-properties (cadr ipoint) (cddr ipoint)
				 '(display)))))))

(defun diaspora-get-all-regexp-markdown-points (regexp)
  (cond ((search-forward-regexp regexp (point-max) t)
	 (cons (cons (match-string-no-properties 2) 
	       (cons (match-beginning 0) 
		     (match-end 0)))
	 (diaspora-get-all-regexp-markdown-points regexp)))
	(t nil)))



(defun diaspora-get-all-image-links ()
  (flet ((d-find-aux ()
		       (cond ((search-forward-regexp diaspora-regexp-image (point-max) t)
			      (cons (match-string-no-properties 2)
				    (d-find-aux)))
			     (t nil))))
    (remove-duplicates (d-find-aux) :test 'equal)))


(provide 'diaspora-stream)
