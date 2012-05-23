
    ;; diaspora-new.el
    ;; Copyright (C) 2012  Giménez, Christian N.

    ;; This program is free software: you can redistribute it and/or modify
    ;; it under the terms of the GNU General Public License as published by
    ;; the Free Software Foundation, either version 3 of the License, or
    ;; (at your option) any later version.

    ;; This program is distributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ;; GNU General Public License for more details.

    ;; You should have received a copy of the GNU General Public License
    ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

    ;; Lunes 12 De Marzo Del 2012    

(require 'cl)

(defvar diaspora-single-message-buffer "*diaspora single message*"
  "Name of the buffer for a diaspora message")

(defun diaspora-interlace-cars (lst1 lst2)
  (if lst1
      (cons (list (car lst1) 
		   (car lst2))
	     (diaspora-interlace-cars (cdr lst1) (cdr lst2)))
    nil))


(defun diaspora-get-url(url)
  "Get a diaspora URL and leave it in a new buffer."
  (let ((url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")
	   ("Accept-Language" . "en")
	   ("Accept-Charset" . "UTF-8"))))
    (url-retrieve-synchronously url)))


(defun diaspora-json-read-url (url)
  "Returns a JSON parsed string from URL."
  (interactive)
  (let ((json-array-type 'list)
	(json-object-type 'alist)
	(http-buffer (diaspora-get-url url)))
    (with-current-buffer http-buffer
      (diaspora-delete-http-header)
      (let ((stream-parsed (json-read)))
	(diaspora-kill-buffer-safe)
	stream-parsed))))

(defun diaspora-parse-json-read (key &optional url)
"Get from URL the JSON part of KEY."
;  (window-configuration-to-register diaspora-stream-register)
  (let ((lst-stream (diaspora-extract-json key
					   (diaspora-json-read-url  
					    (if url
						url
					      diaspora-entry-stream-url))))
	(stream-buffer (get-buffer-create diaspora-stream-buffer)))
    (switch-to-buffer stream-buffer)
    (delete-region (point-min) (point-max))
    (diaspora-header-stream)
    (save-excursion
    ; Instead of (mapcar 'diaspora-show-json-parsed-message lst-stream)
    ; which is must faster!
      (dolist (stream-posts lst-stream)
	(diaspora-show-json-parsed-message stream-posts)))))


(defun diaspora-show-json-parsed-message (parsed-message)
  "This is highly dependend on diaspora* API" 
    (with-current-buffer (current-buffer)
      (let* ((message-id (diaspora-extract-json 'id parsed-message))
	     (message-guid (diaspora-extract-json 
			    'guid parsed-message))
	     (user-id (diaspora-extract-json-list 
		       '(author id) parsed-message))
	     (user-guid (diaspora-extract-json-list 
			 '(author guid) parsed-message))
	     (name (diaspora-extract-json-list 
		    '(author name) parsed-message))
	     (diaspora_id (diaspora-extract-json-list 
			   '(author diaspora_id) parsed-message))
	     (text (diaspora-extract-json-list 
		    '(text) parsed-message))
	     (date  (diaspora-extract-json-list 
		     '(created_at) parsed-message))
	     (provider_display_name  (diaspora-extract-json-list 
		     '(provider_display_name) parsed-message))
	     (avatar (diaspora-extract-json-list
		      '(author avatar small) parsed-message))
	     (photos (diaspora-extract-json-list '(sizes large) 
						 (car (diaspora-extract-json
						       'photos parsed-message))))
	     (amount-comments (diaspora-extract-json-list
			       '(comments_count) parsed-message))
	     (amount-likes (diaspora-extract-json-list
			    '(likes_count) parsed-message)))
	
	(insert  "\n---\n"
		 "![" name "](" avatar ")\n"
		 (format "%s (%s):\n %s\n" name diaspora_id date)
		 (format "user guid: %s\n" user-guid)
		 (format "user id: %s\n" user-id)
		 (format "message id: %s\n" message-id)
		 (format "message guid: %s\n" message-guid)
		 (format "from: %s\n" provider_display_name)
;		 (format "%s\n" date)
		 (format "Has %s comments  and %s likes.\n" amount-comments amount-likes)
		 (format "%s\n\n" text))
	(if photos
	    (insert (format "![photo](%s)" photos))
	  nil))))


;(defun diaspora-show-json-parsed-single-message ()
;  (window-configuration-to-register diaspora-single-message-register)
;  )

;(defun diaspora-show-json-parsed-single-message-comments ()
;  )

(defun diaspora-stream ()
  (interactive)
  (diaspora-ask)
  (diaspora-authenticity-token diaspora-sign-in-url) 
  (diaspora-parse-json-read 'posts)
  (diaspora-mode))


(defun diaspora-see-regexp-markdow ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
					; tag stream
    (let ((markdown-points (diaspora-get-all-regexp-markdown-points  diaspora-regexp-tag 0)))
      (dolist (mpoint markdown-points)
	(add-text-properties (cadr mpoint) (cddr mpoint)
			     (list 'mouse-face 'diaspora-mouse-highlight-face
				   'face "link"
				   'keymap diaspora-show-tag-map
				   'diaspora-tag (car mpoint)
				   'help-echo "Click here to see the tag stream in new buffer.")))))
  
  (save-excursion
    (goto-char (point-min))
					; single message
    (let ((markdown-points (diaspora-interlace-cars
			    (diaspora-get-all-regexp-markdown-points  "\\(message guid: \\)\\(.*\\)" 0)
			    (diaspora-get-all-regexp-markdown-points  "\\(message guid: \\)\\(.*\\)" 1))))
      (dolist (mpoint markdown-points)
	(add-text-properties (cadar mpoint) (cddar mpoint)
			     (list 'mouse-face 'diaspora-mouse-highlight-face
				   'face "link"
				   'keymap diaspora-show-message-map
				   'diaspora-message-id (caadr mpoint)
				   'help-echo "Click here to see the this entry with comments.")))))
  (save-excursion
    (goto-char (point-min))
					; user stream
    (let ((markdown-points (diaspora-interlace-cars 
 			    (diaspora-get-all-regexp-markdown-points  diaspora-regexp-user-entry 0)
			    (diaspora-get-all-regexp-markdown-points  diaspora-regexp-user-id 1))))
      (dolist (mpoint markdown-points)
	(add-text-properties (cadar mpoint) (cddar mpoint)
			     (list 'mouse-face 'diaspora-mouse-highlight-face
				   'face "link"
				   'keymap diaspora-show-guid-user-map
				   'diaspora-user-guid (caadr mpoint)
				   'help-echo "Click here to see this user stream in new buffer."))))))
  
(defvar diaspora-show-message-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'diaspora-show-single-message)
    (define-key map [mouse-2] 'diaspora-show-single-message)
    map)
  "")





(defun diaspora-show-single-message ()
  (let ((message-id
	 (get-text-property (+ 1 (previous-single-property-change (point) 'diaspora-message-id))
			    'diaspora-message-id)))
    (diaspora-get-single-message message-id)))

;(diaspora-get-single-message "1263970")


(defun  diaspora-get-guid-user-stream (guid)
  ""
  (interactive)  
  (diaspora-ask)
  (diaspora-authenticity-token diaspora-sign-in-url)
  (save-excursion
    (diaspora-parse-json-read 'posts  (concat "https://" diaspora-pod "/people/" guid ".json"))
    (diaspora-mode)))



(defun diaspora-get-entry-stream-tag (tag)
  ""
  (interactive "M")
    (diaspora-ask)
    (diaspora-authenticity-token diaspora-sign-in-url)
    (save-excursion
      (diaspora-parse-json-read 'posts  (concat "https://" diaspora-pod "/tags/" tag ".json"))
      (diaspora-mode)))

(defun diaspora-get-single-message (message-id &optional commentp)
  "This does not work..."
  (interactive)
  (window-configuration-to-register diaspora-single-message-register)
  (diaspora-ask)
  (diaspora-authenticity-token diaspora-sign-in-url)
  (save-excursion
    (let ((buffer-message (get-buffer-create diaspora-single-message-buffer)))
      (switch-to-buffer buffer-message)
      (with-current-buffer buffer-message
	(diaspora-show-json-parsed-message 
	 (car (diaspora-json-read-url
	       (format "%s/%s.json" diaspora-single-message-url message-id))))
	(switch-to-buffer-other-window buffer-message)
;    (concat "https://" diaspora-pod "/" message-id ".json"))))
	(diaspora-mode)))))
      
(defvar diaspora-regexp-user-guid "^user guid: \\(.*\\)"
  )

(defvar diaspora-regexp-user-id "^user id: \\(.*\\)"
  )

(defvar diaspora-regexp-message-guid "^message guid: \\(.*\\)"
  )
(defvar diaspora-regexp-message-id "^message id: \\(.*\\)"
  )

(defun diaspora-shortenurl-replace-at-point ()
  (interactive)
  "Replace the url at point with a tiny version."
  (interactive)
  (let ((url-bounds (bounds-of-thing-at-point 'url)))
    (when url-bounds
      (let ((url (short-url (thing-at-point 'url))))
	(when url
	  (save-restriction
	    (narrow-to-region (car url-bounds) (cdr url-bounds))
	    (delete-region (point-min) (point-max))
	    (insert url)))))))

(defun diaspora-post-comment (post id)
  "Does not work!!!
Post POST to diaspora."
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		    (list (cons "user[username]" diaspora-username)
			  (cons "user[password]" diaspora-password)
			  (cons "authenticity_token" diaspora-auth-token)
			  (cons "text" post)
			  (cons (concat "new_comment_on_" id) id)
			  (cons (concat "comment_text_on_" id) id))
		    "&")))
    (url-retrieve (concat "https://joindiaspora.com/posts/" id)
		  (lambda (arg) 
		    (switch-to-buffer (current-buffer))))))

;action="/posts/{{id}}/comments" class="new_comment" id="new_comment_on_{{id}}" method="post">
;      {{#with current_user}}

;(diaspora-authenticity-token "https://joindiaspora.com/posts/1257932")
;(diaspora-post-comment "no-comments" "1257932")

;(concat "https://joindiaspora.com/posts/" "1257932")
;"https://joindiaspora.com/posts/1257932"

				 


(defvar diaspora-show-tag-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'diaspora-show-tag-stream)
    (define-key map [mouse-2] 'diaspora-show-tag-stream)
    map)
  "")


(defun diaspora-show-tag-stream (&rest r)
  (interactive)
  (let ((tag
	 (get-text-property (+ 1 (previous-single-property-change (point) 'diaspora-tag))
			    'diaspora-tag)))
    (diaspora-get-entry-stream-tag  (diaspora-markdown-tag-strip tag))))


(defvar diaspora-show-guid-user-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'diaspora-show-user-stream)
    (define-key map [mouse-2] 'diaspora-show-user-stream)
    map)
  "")

(defvar diaspora-show-video-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'goto-address-at-point)
    (define-key map [mouse-2] 'goto-address-at-point)
    map)
  "")


(defun diaspora-header-stream ()
  (insert (propertize
  	   (format "%s" "Extrapolation")
  	   'mouse-face 'diaspora-mouse-highlight-face
  	   'face diaspora-header-face-1
  	   'keymap diaspora-stream-header-map
  	   'help-echo "Click here to see this diaspora stream.")
  	  " ")
  (insert (propertize
  	   (format "%s" "Pinboard")
  	   'mouse-face 'diaspora-mouse-highlight-face
  	   'face diaspora-header-face-1
  	   'keymap diaspora-likes-header-map
  	   'help-echo "Click here to see this diaspora stream.")
  	  " ")
  (insert
	  (propertize
	   (format "%s" "search tag")
	   'mouse-face 'diaspora-mouse-highlight-face
	   'face diaspora-header-face-1
	   'keymap diaspora-stream-header-tag-map
	   'help-echo "Click here to see this diaspora stream tag (tag will be prompted).")))


(defvar diaspora-stream-header-tag-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'diaspora-get-entry-stream-tag)
    map)
  "")


(defvar diaspora-likes-header-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'diaspora-stream-likes)
    map)
  "")


(defvar diaspora-stream-header-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'diaspora-stream)
    map)
  "")


(defun diaspora-show-user-stream (&rest r)
  (interactive)
  (let ((guid-user
	 (get-text-property (+ 1 (previous-single-property-change (point) 'diaspora-user-guid))
			    'diaspora-user-guid)))
    (diaspora-get-guid-user-stream  guid-user)))

(defun diaspora-show-videos ()
  (interactive)
  (let ((markdown-points (diaspora-get-all-regexp-markdown-points diaspora-regexp-youtube-link)))
    (dolist (mpoint markdown-points)
      (add-text-properties (cadr mpoint) (cddr mpoint)
			   (list 'mouse-face 'diaspora-mouse-highlight-face
				 'face "link"
				 'keymap diaspora-show-video-map
				 'help-echo "Click here to see the this video on a external browser.")))))




(provide 'diaspora-new)


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
    (diaspora-kill-buffer-safe 
     (url-retrieve-synchronously (diaspora-post-comment-url post-id)))))

(defun diaspora-post-comment-url (post-id)
  "Return the URL for posting a comment for the post with id post-id"
  (diaspora-url
   (format "%s/%s/%s"
	   diaspora-single-message-url
	   (if (numberp post-id)
	       (number-to-string post-id)
	     post-id)
	   diaspora-comment-name)))

(defconst diaspora-comment-buffer-name "*diaspora comment*"
  "This is the name of the comment buffer.")

(defvar diaspora-comment-buffer nil
  "This is the buffer (supposed to be only one or unique) for write a comment.")

(defun diaspora-new-comment-buffer (post-id)
  "Create a new buffer for write a comment for the post with id given by post-id."
  (interactive)
  (setq diaspora-next-comment-to-post post-id)
  ;; create buffer
  (setq diaspora-comment-buffer (get-buffer-create diaspora-comment-buffer-name))
  (switch-to-buffer-other-window diaspora-comment-buffer)
  ;; insert header and footer, modes... etc.
  (diaspora-date)
  (insert diaspora-footer-post)
  (goto-char (point-min))
  (insert diaspora-header-post)
  (diaspora-mode)
  (message "Use C-c C-c to comment to diaspora or use diaspora-send-comment-this-buffer."))

(defvar diaspora-next-comment-to-post
  nil
  "This is the post id where to send the comment in the next `diaspora-send-comment-this-buffer' function call.")

(defun diaspora-url (location)
  "Make the URL according to the `diaspora-pod'(pod selected)."
  (format "%s://%s/%s"
	  (if t
	      "https"
	    "http")
	  diaspora-pod
	  location))

(defcustom diaspora-comment-name
  "comments"
  "This is the name of the comments for posting."
  :type 'string
  :group 'diaspora-streams)

(defun diaspora-send-comment-this-buffer ()
  "Send this buffer as a comment to the post determined by the id `diaspora-next-comment-to-post'."
  (interactive)
  (diaspora-ask)
  (when (null diaspora-auth-token)
    (message (concat "Getting authenticity token..."))
    (diaspora-authenticity-token (diaspora-url diaspora-sign-in-url))
    (message (concat "done: " diaspora-auth-token)))
  (diaspora-send-comment-post (buffer-string) diaspora-next-comment-to-post))