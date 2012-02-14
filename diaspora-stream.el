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

;; Streaming 

(require 'diaspora-comments)

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
  "Get the Diáspora URL and leave it in a new buffer.
Returns: A new buffer where is all the information retrieved from the URL."
  (let ((url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")
	   ("Accept-Language" . "en")
	   ("Accept-Charset" . "utf-8")))
	(buffer-file-coding-system 'utf-8))
    (url-retrieve-synchronously url)))

(defun diaspora-delete-http-header ()
  "Delete the first lines that is the HTTP header in the current buffer.
This is used after getting a stream or any URL in JSON format."
   (goto-char (point-min))
   (search-forward "\n\n")      
   (delete-region (point-min) (match-beginning 0)))

(defun diaspora-get-stream-by-name (stream-name)
  "I try to get the stream given a name, and then show it parsed in a new buffer.
 This means, I format the URL according to this rules:

1) I add the pod URL.
2) I add the stream-name 
3) I add the extension \".json\".

For example:
if the `diaspora-pod' has the value: \"joindiaspora.com\", then
  (diaspora-get-stream-by-name 'aspects')

will get the https://joindiaspora.com/aspects.json URL, parse it, and show it in a new buffer."
  (interactive "MName of the stream?")
  (diaspora-get-stream 
   (diaspora-url-json stream-name)))

(defun diaspora-get-stream(stream-url)
  "Get the stream given by the url, and then, show it in the diaspora buffer.
I expect to be logged in, but if not, I download the authenticity token."  
  (diaspora-ask) ;; don't forget username and password!
  (when (null diaspora-auth-token)
    (diaspora-authenticity-token (diaspora-url diaspora-sign-in-url))) ;; Get the authenticity token    
  ;; get the in JSON format all the data
  (let ((buff (diaspora-get-url-entry-stream stream-url)))
    (with-current-buffer buff
      ;; Delete the HTTP header...
      (diaspora-delete-http-header)
      ;; Parse JSON...
      (diaspora-parse-json)
      ;;Change markdown to html... not so good.      
      ;;(diaspora-change-to-html)
      ;;Better using diaspora-mode already done by Tiago!      
      (diaspora-mode) 
      (if diaspora-show-images-by-default
	  (progn
	    (diaspora-get-all-images)
	    (diaspora-show-images)
	    )
	(goto-char (point-min))
	)
    ;; Delete HTTP Buffer
    ;;(kill-buffer buff)
    )))

					; Streams!

(defun diaspora-get-participate-stream ()
  "Show the participate stream."
  (interactive)
  (diaspora-get-stream-by-name diaspora-participate-stream-name))  

(defun diaspora-get-entry-stream ()
  "Show the entry stream. 
First look for the JSON file at `diaspora-entry-stream-url' and then parse it.
I expect to be already logged in. Use `diaspora' for log-in."
  (interactive)  
  (diaspora-get-stream-by-name diaspora-entry-stream-url)
  )

(defun diaspora-get-public-stream ()
  "Show the public stream."
  (interactive)
  (diaspora-get-stream-by-name diaspora-public-stream-name))

(defun diaspora-get-followed-tags-stream ()
  "Show the followed tags stream."
  (interactive)
  (diaspora-get-stream-by-name diaspora-followed-tags-stream-name))

(defun diaspora-get-mentions-stream ()
  "Show the mentions stream."
  (interactive)
  (diaspora-get-stream-by-name diaspora-mentions-stream-name))

(defun diaspora-get-liked-stream ()
  "Show the liked stream."
  (interactive)
  (diaspora-get-stream-by-name diaspora-liked-stream-name))

(defun diaspora-get-commented-stream ()
  "Show the commented stream."
  (interactive)
  (diaspora-get-stream-by-name diaspora-commented-stream-name)
  )

(defun diaspora-get-aspects-stream ()
  "Show the aspects stream."
  (interactive)
  (diaspora-get-stream-by-name diaspora-aspects-stream-name))
 

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
    (define-key map [c] 'diaspora-comment-message-new-buffer)
    (define-key map [return] 'diaspora-show-message-new-buffer)
    (define-key map [mouse-2] 'diaspora-show-message-new-buffer)
    map)
  "Keymap used when the user clics on a name link.")

(defvar diaspora-stream-message-map
  (let ((map (make-sparse-keymap)))
    (define-key map [\C-q] 'diaspora-single-message-destroy)
    map)
  "Keymap used in the stream and messages buffers.")


;; A few notes about the next functiom `diaspora-show-message`
;; date: 20120128
;;
;; It would be much easier not to insert the text with properties as is done
;; I think it is preferable to add the properties latter on; using the same type
;; of procedures that is used to insert images. Just a thought.


(defun diaspora-show-message (parsed-message &optional buffer)
  "Show a parsed message in a given buffer.
If buffer is nil, then use the `current-buffer'."
  ;; Ensure that buffer is not nil, in case is nil, buffer will be `current-buffer'.
;; debug
;  (setq aux  parsed-message)
  (let ((buffer (if (null buffer)
		    (current-buffer)
		  buffer)))
    (with-current-buffer buffer
      (let* ((id (cdr (assoc 'id parsed-message)))
	     (name (diaspora-extract-json-list 
		    '(author name) parsed-message))
	     (diaspora_id (diaspora-extract-json-list 
			   '(author diaspora_id) parsed-message))
	     (text (diaspora-extract-json-list 
		    '(text) parsed-message))
	     (date  (diaspora-extract-json-list 
		     '(created_at) parsed-message))
	     (avatar (diaspora-extract-json-list
		      '(author avatar small) parsed-message))
	     (photos (cdr (assoc 'photos parsed-message)))
	     (amount-comments (diaspora-extract-json-list
			       '(comments_count) parsed-message))
	     (amount-likes (diaspora-extract-json-list
			    '(likes_count) parsed-message)))
	
	(insert  "---\n")
	(insert "![" name "](" avatar ")\n")
	(insert (diaspora-add-link-to-publication 
		 (format "%s (%s):\n" name diaspora_id) 
		 id))
	(insert (format "%s\n" date))
	(insert (format "Has %s comments. %s likes.\n" amount-comments amount-likes))
	(insert (format "%s\n\n" text))
	(if (equal (length photos) 0) ""
	  (insert "![photo](" 
		  (cdr (assoc 'large (car (aref (cdr (assoc 'photos parsed-message))0))))
		  ")\n"))))))

(defun diaspora-add-link-to-publication (text id-message)
  "Return a propertized text with a link to publication. Ready to use with a map like `diaspora-show-message-map'
or a function like `diaspora-show-message-new-buffer'."
  (propertize
   text
   'mouse-face 'highlight
   'face "link"
   'keymap diaspora-show-message-map
   'diaspora-id-message id-message
   'help-echo "Click here to see this message in new buffer.")
  )

(defun diaspora-get-id-message-near-point ()
  "Get the diaspora-id-message property value searching from point.
Use it for getting the nearest id post number when selecting a message."
  (get-text-property (+ 1 (previous-single-property-change (+ (point) 1) 'diaspora-id-message))
		     'diaspora-id-message))


(defun diaspora-show-message-new-buffer (&rest r)
  "Show this message in new buffer. Load the message, and all its comments, and show it!."
  (interactive)
  (diaspora-get-single-message (diaspora-get-id-message-near-point)))

(defun diaspora-comment-message-new-buffer (&rest r)
  "Create a new buffer for commenting the current message."
  (interactive)
  (diaspora-new-comment-buffer (diaspora-get-id-message-near-point)))

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
		    (format "%s/%s.json" (diaspora-url diaspora-single-message-url) id-message))))
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
    (let ((json-array-type 'list)
	  (json-object-type 'alist)
	  (lstparsed (cdr (assoc 'posts (json-read)))))
      (with-current-buffer buff-to
	;; Clean buffer buff-to and insert message
	(delete-region (point-min) (point-max))
	(diaspora-show-message lstparsed)))))

(defun diaspora-parse-json (&optional status)
  "Parse de JSON entry stream."
  (goto-char (point-min))
    (window-configuration-to-register diaspora-stream-register)
  ;; Create a new buffer called according `diaspora-buffer' say 
  ;; and parse the json code into lists.
  (let ((json-array-type 'list)
       (json-object-type 'alist)
       (lstparsed (cdr (assoc 'posts (json-read))))
       (buff (get-buffer-create diaspora-stream-buffer)))
    ;; clean the new buffer
    (switch-to-buffer buff)
    (let ((le (length lstparsed)))
	  ;(inhibit-read-only t))
      (delete-region (point-min) (point-max))
    ;; Show all elements
      (dotimes (i le)
	(diaspora-show-message (aref lstparsed i) buff)))))

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
	 (concat diaspora-image-directory
		 (if user-id 
		     (concat user-id "-"))
		 (file-name-nondirectory url))))
    (setq buffer-file-coding-system 'no-conversion)
    (setq buffer-file-name image-file-name)
    (goto-char (point-min))
    (delete-region (point-min) (search-forward "\C-j\C-j"))
    (save-buffer 0)
    (kill-buffer (current-buffer))))

(defun diaspora-get-all-images ()
  (interactive)
  (mapcar 'diaspora-get-image (diaspora-get-all-image-links)))

(defun diaspora-show-images ()
  "Shows images in buffer."
  (interactive)
  (save-excursion
    (let ((buffer-undo-list t)
	  (inhibit-read-only t)
	  (inhibit-point-motion-hooks t)
	  (inhibit-modification-hooks t)
	  (modified-p (buffer-modified-p))
	  deactivate-mark
	  (images-points (diaspora-get-all-regexp-markdown-points diaspora-regexp-image)))
      (dolist (ipoint images-points)
	(diaspora-insert-image (cadr ipoint) (cddr ipoint)))
      (goto-char (point-min)))))

(defun diaspora-insert-image (beg end)
  "Create an image  and insert it place of an `diaspora-regexp-image' defined by BEG and END."
  (add-text-properties (cadr ipoint) (cddr ipoint)
		       (list 'display (create-image 
				       (concat diaspora-image-directory
					       (file-name-nondirectory 
						(car ipoint)))))))
(defun diaspora-unshow-images ()
  "Un shows images in buffer."
  (interactive)
  (save-excursion
    (let ((buffer-undo-list t)
	  (inhibit-read-only t)
	  (inhibit-point-motion-hooks t)
	  (inhibit-modification-hooks t)
	  (modified-p (buffer-modified-p))
	  deactivate-mark)
      (unwind-protect
	  (remove-text-properties (point-min) (point-max)
				  '(display)))
      (set-buffer-modified-p modified-p))))

(defun diaspora-get-all-regexp-markdown-points (regexp &optional opt)
  (cond ((search-forward-regexp regexp (point-max) t)
	 (cons (cons (match-string-no-properties 
		      (if (not opt) 2
			opt))
	       (cons (match-beginning 0) 
		     (match-end 0)))
	 (diaspora-get-all-regexp-markdown-points regexp
						  (if (not opt) 2
						    opt))))
	(t nil)))

(defun diaspora-show-videos (&optional opt)
  ""
  (interactive)
  (goto-char (point-min))
  (save-excursion
    (let ((images-points (diaspora-get-all-regexp-markdown-points diaspora-regexp-youtube-link)))
      (dolist (ipoint images-points)
	(if (not opt)
	    (add-text-properties (cadr ipoint) (cddr ipoint)
				 (list 'display (create-image 
						 (concat diaspora-user-image-dir "/" 
							 "video.png"))))
	  (remove-text-properties (cadr ipoint) (cddr ipoint)
				  '(display)))))))



(defun diaspora-get-all-image-links ()
  (goto-char (point-min))
  (save-excursion
    (flet ((d-find-aux ()
		       (cond ((search-forward-regexp diaspora-regexp-image (point-max) t)
			      (cons (match-string-no-properties 2)
				    (d-find-aux)))
			     (t nil))))
      (remove-duplicates (d-find-aux) :test 'equal))))

(defun diaspora-see-regexp-markdow ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((markdown-points (diaspora-get-all-regexp-markdown-points  diaspora-regexp-tag 0)))
      (dolist (mpoint markdown-points)
	(add-text-properties (cadr mpoint) (cddr mpoint)
			     (list 'mouse-face 'highlight
				   'face "link"
				   'keymap diaspora-show-tag-map
				   'diaspora-tag (car mpoint)
				   'help-echo "Click here to see the tag stream in new buffer."))
	))))

    ;; (goto-char (point-min))
    ;; (let ((markdown-points (diaspora-get-all-regexp-markdown-points  diaspora-regexp-user-entry 0)))
    ;;   (dolist (mpoint markdown-points)
    ;; 	(add-text-properties (cadr mpoint) (cddr mpoint)
    ;; 			     (list 'mouse-face 'highlight
    ;; 				   'face "link"
    ;; 				   'keymap diaspora-show-message-map
    ;; 				   'diaspora-id-message id
    ;; 				   'help-echo "Click here to see this message in new buffer."))
    ;; 	))))
    
;; (insert (propertize
	;; 	 (format "%s(%s):\n" name diaspora_id)
	;; 	 'mouse-face 'highlight
	;; 	 'face "link"
	;; 	 'keymap diaspora-show-message-map
	;; 	 'diaspora-id-message id
	;; 	 'help-echo "Click here to see this message in new buffer."))

(defun diaspora-show-tag-new-buffer (&rest r)
  (interactive)
  (let ((tag
	 (get-text-property (+ 1 (previous-single-property-change (point) 'diaspora-tag))
			    'diaspora-tag)))
    (diaspora-get-entry-stream-tag  (diaspora-markdown-tag-strip tag))))

(defvar diaspora-show-tag-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'diaspora-show-tag-new-buffer)
    (define-key map [mouse-2] 'diaspora-show-tag-new-buffer)
    map)
  "")

;; TODO
;; diaspora-markdown-strip
;; diaspora-html-strip

(defun diaspora-markdown-tag-strip (string)
  (save-match-data
    (if (string-match "#\\([a-zA-Z0-9_/\.-]+\\)" string)
        (match-string 1 string)
      string)))

(defun diaspora-html-strip-links (string)
  "Remove all HTML links from STRING."
  (replace-regexp-in-string "\\(<a .*?>\\|</a>\\)" "" string nil t))

;; Functions to extract content from json-read
;; They are, probably, done some where else...but I don't no where
;; so there you have them.


(defun diaspora-extract-json (e a)
  (cdr (assoc e a)))

(defun diaspora-extract-json-list (e a)
  (cond (e
	 (diaspora-extract-json-list (cdr e) 
			 (diaspora-extract-json (car e) a)))
	(a)))

(defun diaspora-get-stream-by-tag (tag)
  "Get a stream of the messages with the tag given by 'tag'.
The tag must be a string without the starting \"#\"."
  (interactive "MTag(without '#')?")
  (diaspora-get-stream-by-name (format "/tags/%s" tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun diaspora-get-url(url)
  "Get a diaspora URL and leave it in a new buffer."
  (let ((url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")
	   ("Accept-Language" . "en")
	   ("Accept-Charset" . "UTF-8"))))
    (url-retrieve-synchronously url)))


(defun diaspora-inspect-json (env)
  (flet ((f-car (lst)
		(cond ((listp lst)
		       (if (listp (car lst))
			   (mapcar 'f-car lst)
			 (f-car (car lst))))
		      (t 
		       lst))))
    (cond ((listp env)
	   (mapcar 'f-car env))
	  (t
	   env))))

(defun diaspora-json-read-url (url)
  "Returns a JSON parsed string from URL."
  (interactive)
  (let ((json-array-type 'list)
	(json-object-type 'alist)
	(http-buffer (diaspora-get-url url)))
    (with-current-buffer http-buffer
      (diaspora-delete-http-header)
      (let ((stream-parsed (json-read)))
	 stream-parsed))))

(defsubst diaspora-string-trim (string)
  "Remove leading and trailing whitespace and all properties from STRING.
If STRING is nil return an empty string."
  (if (null string)
      ""
    (if (string-match "\\`[ \t\n]+" string)
        (setq string (substring string (match-end 0))))
    (if (string-match "[ \t\n]+\\'" string)
        (setq string (substring string 0 (match-beginning 0))))
    (substring-no-properties string)))

(defun diaspora-look-for-aspects ()
  "Search for each aspect name an id and return an alist with all the aspects founded.

We look for the keyword \"data-aspect_id=\" and we are sure that the next line has the name with spaces."
  (goto-char (point-min))
  (let ((lista '()))
    (while (search-forward-regexp "data-aspect_id='?\\([^'> ]*\\)'?" nil t)
      (let ((value (match-string-no-properties 1))
	    (name (progn 
		     (forward-line)
		     (diaspora-string-trim 
		      (buffer-substring-no-properties (point) (point-at-eol))))))
	(push (cons name value) lista)))
    lista))

(defun diaspora-get-aspects (&optional reload)
  "If `diaspora-aspect-alist hasn't been generated, get an alist of aspects as key and id as values from the Diaspora pod and return the alist.
After generate the alist, save it in `diaspora-aspect-alist'.
If the reload parameter is t then, no matter what `diaspora-aspect-alist' has, reload from the `diaspora-bookmarklet-location' URL."
  (if (or reload
	  (null diaspora-aspect-alist))
      (progn 
	;; We haven't loaded the aspects yet. Load it!
	(with-current-buffer (diaspora-get-url-entry-stream (diaspora-url diaspora-bookmarklet-location))
	  (setq diaspora-aspect-alist (diaspora-look-for-aspects))))
    diaspora-aspect-alist))

(provide 'diaspora-stream)
