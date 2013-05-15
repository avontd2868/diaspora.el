;;; diaspora-stream.el --- Simple Emacs-based client for diaspora*

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

(require 'cl)
(require 'diaspora-comments)
(require 'diaspora-http-errors)

					; ********************
					; Customization

(defgroup diaspora-streams nil
  "URL and names for the Streams used in diaspora.el."
  :group 'diaspora-urls
  :version "23.0"
  :tag "diaspora streams urls")

(defcustom diaspora-timezone -3
  "Amount of hours as a timezone. If your timezone is -3UTC then use -3(three hours less to reach the UTC!)"
  :group 'diaspora
  :type 'integer)

(defcustom diaspora-get-always-authenticity-token t
  "Always get the authenticity token when connecting to Diáspora. 
You may would like to get only one authenticity token, but sometimes posting or getting info may fail.

If you set this to true, then diaspora.el will always get the authenticity token, making it more slow but robust.

Note: If you have a slow Internet, you may would like to set this into false(or nil)."
  :group 'diaspora-streams
  :type 'boolean)

(defcustom diaspora-participate-stream-name
  "participate"
  "Name of the \"Participate\" stream. 
This is the name of the page, for example:
If `diaspora-pod' has the value \"joindiaspora.com\", then,
the JSON page is at the URL:
  https://joindiaspora.com/participate.json

And the `diaspora-participate-stream-name' must be at value \"participate\"."
  :type 'string
  :group 'diaspora-streams)

(defcustom diaspora-explore-stream-name
  "explore"
  "This is the name (as appear in diaspora/config/routes.rb in the diaspora project) of the entry stream or explore stream.
This is the name of the page, for example:
If `diaspora-pod' has the value \"joindiaspora.com\", then,
the JSON page is at the URL:
  https://joindiaspora.com/participate.json

And the `diaspora-participate-stream-name' must be at value \"participate\"."
  :type 'string
  :group 'diaspora-streams)

(defcustom diaspora-entry-stream-name 
  "stream"
  "JSON version of the entry stream(the main stream)."
  :type 'string
  :group 'diaspora-streams)

(defcustom diaspora-public-stream-name
  "public"
  "This is the name (as appear in diaspora/config/routes.rb in the diaspora project) of the public stream.
This is the name of the page, for example:
If `diaspora-pod' has the value \"joindiaspora.com\", then,
the JSON page is at the URL:
  https://joindiaspora.com/participate.json

And the `diaspora-participate-stream-name' must be at value \"participate\"."
  :type 'string
  :group 'diaspora-streams)

(defcustom diaspora-followed-tags-stream-name
  "followed_tags"
  "This is the name (as appear in diaspora/config/routes.rb in the diaspora project) of the followed tags stream.
This is the name of the page, for example:
If `diaspora-pod' has the value \"joindiaspora.com\", then,
the JSON page is at the URL:
  https://joindiaspora.com/participate.json

And the `diaspora-participate-stream-name' must be at value \"participate\"."
  :type 'string
  :group 'diaspora-streams)

(defcustom diaspora-mentions-stream-name
  "mentions"
  "This is the name (as appear in diaspora/config/routes.rb in the diaspora project) of the mentions stream.
This is the name of the page, for example:
If `diaspora-pod' has the value \"joindiaspora.com\", then,
the JSON page is at the URL:
  https://joindiaspora.com/participate.json

And the `diaspora-participate-stream-name' must be at value \"participate\"."
  :type 'string
  :group 'diaspora-streams)

(defcustom diaspora-liked-stream-name
  "liked"
  "This is the name (as appear in diaspora/config/routes.rb in the diaspora project) of the liked stream.
This is the name of the page, for example:
If `diaspora-pod' has the value \"joindiaspora.com\", then,
the JSON page is at the URL:
  https://joindiaspora.com/participate.json

And the `diaspora-participate-stream-name' must be at value \"participate\"."
  :type 'string
  :group 'diaspora-streams)

(defcustom diaspora-commented-stream-name
  "commented"
  "This is the name (as appear in diaspora/config/routes.rb in the diaspora project) of the commented stream.
This is the name of the page, for example:
If `diaspora-pod' has the value \"joindiaspora.com\", then,
the JSON page is at the URL:
  https://joindiaspora.com/participate.json

And the `diaspora-participate-stream-name' must be at value \"participate\"."
  :type 'string
  :group 'diaspora-streams)

(defcustom diaspora-image-external-program "eog"
  "This is the program path and name. If you want to see an image in an external program this must be
setted correctly."
  :group 'diaspora
  :type 'string)

					; ********************
					; Constants  

(defconst diaspora-stream-buffer "*diaspora stream*"
  "The name of the diaspora stream buffer.")

					; ********************
					; Internal Variables

(defvar diaspora-stream-last-post-date nil
  "A list with two (or more) elements in the format like `current-time'.

  (HIGH LOW MICROSECOND)
Where HIGH are the 16 bits most significant bit values and LOW are the 16 bits least significant bit values. 
MICROSECOND are ignored, even can be absent."
)

(defvar diaspora-last-stream-visited nil
  "A list with two elements: the stream name and the max-time.

If max-time is nil, then the max-time is the current-time.

If this variable is nil then there was no last stream visited.")

					; ********************
					; Functions

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
    (diaspora-kill-buffer-safe buf-kill)))

(defun diaspora-get-time-by-timezone (max-time)
  "Return the time in seconds from the epoch modified according to the timezone specified by `diaspora-timezone' to represents the time
in the UTC standard.

This is usefull for giving this as a GET(or POST) \"max_time\" parameter for any stream."
  (+ (float-time max-time) (* diaspora-timezone 3600)) ;; 3600 is one hour.
  )


(defun diaspora-get-url-entry-stream (url &optional max-time lst-get-parameters lst-post-parameters)
  "Get the Diáspora URL and leave it in a new buffer.
Returns: A new buffer where is all the information retrieved from the URL.
nil if something happened."
  (let ((url-registered-auth-schemes '())
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")
	   ("Accept-Language" . "en")
	   ("Accept-Charset" . "utf-8")))
	(buffer-file-coding-system 'utf-8))
    (if max-time
	
	(let ((url-request-data ;; the interval of time has been setted
	       (mapconcat (lambda (arg)
			    (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
			  (append (list (cons "max_time" (number-to-string (diaspora-get-time-by-timezone max-time))))
				  lst-get-parameters
				  lst-post-parameters)			  
			  "&"))
	      )

	  (diaspora-debug-msg "***GETing:")
	  (diaspora-debug-msg url)
	  (diaspora-debug-msg url-request-data)
	  
	  (let ((out-buffer (url-retrieve-synchronously url))
		)
	    (diaspora-stream-check-http-error)
	  
	    out-buffer ;; Ensure that the output is the buffer returned by `url-retrieve-synchronously'.
	    )
	  )      
      (let ((url-request-data ;; there is no interval of time
	     (mapconcat (lambda (arg)
			  (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
			(append lst-get-parameters lst-post-parameters)
			"&"))
	    )

	(diaspora-debug-msg "***GETing:")
	(diaspora-debug-msg url)
	(diaspora-debug-msg url-request-data)

	(let* ((out-buffer (url-retrieve-synchronously url))
	       )
	  (with-current-buffer out-buffer
	    (diaspora-stream-check-http-error)
	    )

	  out-buffer ;; Ensure that the output is the buffer returned by `url-retrieve-synchronously'.
	  )
	)
      )
    )
  )

(defun diaspora-stream-check-http-error ()
  "Check if there is an HTTP error, if there is present an `error' message. If not return nil."  
  (let ((errornum (diaspora-http-error-get-number)))
    (unless (equal errornum 200)
      (cond
       ((equal errornum 404)
	(error "Page not found!" "Is your pod well configured? see `diaspora-pod'" "Check URLs variables(diaspora-*-url)."))
       ((equal errornum 401)
	(error "Unauthorized access!" "Is your username and password correct? use M-x `diaspora-login' function to call for sign in and try again."))
       )
      nil
      )
    )
  )

(defun diaspora-delete-http-header ()
  "Delete the first lines that is the HTTP header in the current buffer.
This is used after getting a stream or any URL in JSON format.
If there is no HTTP header, do nothing."
   (goto-char (point-min))
   (when (search-forward "\n\n" nil t)
     (delete-region (point-min) (match-beginning 0)))
   )

(defun diaspora-get-next-oldies ()
  "Get the next olds post of the last visited stream.

I use the `diaspora-stream-last-post-date' variable.

This is the same as going up to the bottom of the page and let diaspora reload the older posts."
  (interactive)
  (if diaspora-stream-last-post-date
      (progn
	(diaspora-visit-last-stream diaspora-stream-last-post-date)
	)
    (message "You need to get a stream: there is no last stream visited!")
    )
  )

(defun diaspora-visit-last-stream (&optional other-max-time)
  "Visit the last stream, maybe with max-time changed.

OTHER-MAX-TIME is a list with two elements:
  (HIGH LOW)
This is a timestamp as `current-time' returns.

Is used for getting the posts created up to that time.

I use `diaspora-last-stream-visited' variable for getting the name of the last stream visited."
  (interactive)
  (if diaspora-last-stream-visited     
      (diaspora-get-stream-by-name (car diaspora-last-stream-visited) (or other-max-time ;; use other-max-time if setted
									  (nth 1 diaspora-last-stream-visited)))
    (message "There's no last stream visited."))
  )
  

(defun diaspora-get-stream-by-name (stream-name &optional max-time)
  "I try to get the stream given a name, and then show it parsed in a new buffer.
 This means, I format the URL according to this rules:

1) I add the pod URL.
2) I add the stream-name 
3) I add the extension \".json\".

For example:
if the `diaspora-pod' has the value: \"joindiaspora.com\", then
  (diaspora-get-stream-by-name 'aspects')

will get the https://joindiaspora.com/aspects.json URL, parse it, and show it in a new buffer.

MAX-TIME is a time where to fetch the post earlier up to that time . It must be in the format as `current-time'(or `encode-time') returns: a list of three elements(where the third is totally ignored): 
  (HIGH LOW MICROSECOND)
Where HIGH are the 16 bits most significant bit values and LOW are the 16 bits least significant bit values. 
MICROSECOND are ignored, even can be absent."
  (interactive "MName of the stream?")
  (setq diaspora-last-stream-visited (list stream-name max-time))
  (diaspora-get-stream 
   (diaspora-url-json stream-name)
   max-time))

(defun diaspora-get-stream(stream-url max-time &optional lst-get-parameters lst-post-parameters)
  "Get the stream given by the url, and then, show it in the diaspora buffer.
I expect to be logged in, but if not, I download the authenticity token.

Set MAX-TIME with a valid emacs timestamp to fetch information from and until that interval of time.

Use LST-GET-PARAMETERS to give special GET parameters to the STREAM-URL.
Same as LST-POST-PARAMETERS."
  (diaspora-ask) ;; don't forget username and password!
  (diaspora-get-authenticity-token-if-necessary)
  ;; get the in JSON format all the data
  (let* (
	(stream-buff (get-buffer-create diaspora-stream-buffer))
	(lst-get-parameters (append  (list (cons "user[username]" diaspora-username)
					   (cons "user[password]" diaspora-password)
					   (cons "user[remember_me]" "1")
					   (cons "authenticity_token" diaspora-auth-token))
				     lst-get-parameters))
	(buff (diaspora-get-url-entry-stream stream-url max-time lst-get-parameters lst-post-parameters )))
    (unless buff
      (error "I (`diaspora-get-stream') could not find any useful information!")
      )
    (with-current-buffer buff
      ;; Delete the HTTP header...
      (diaspora-delete-http-header)
      ;; Parse JSON...
      (let ((inhibit-read-only t))
	;; Apply diaspora-mode
	(with-current-buffer stream-buff	  
	  (diaspora-mode)
	  (diaspora-stream-mode)
	  
	  (diaspora-parse-json buff stream-buff)

	  (if diaspora-show-images-by-default
	      (progn
		(diaspora-get-all-images)
		(diaspora-show-images)
		)
	    )

	  (diaspora-remove-bad-chars)
	  (diaspora-replace-bad-links)
	  (diaspora-hide-markdown)

	  (switch-to-buffer stream-buff)
	  (goto-char (point-min))
	  )
	)
      )
    ;; Delete HTTP Buffer if `diaspora-debug-mode' is off
    (diaspora-kill-buffer-safe buff)      
    )
  )


(defun diaspora-read-date ()
  "Read a date from the minibuffer and return in the format as `current-time' or `encode-time' does."
  (let ((day 0)
	(month 0)
	(year 0)
	(hour 0)
	(min -1)
	(sec 0)
	(max-year (nth 5 (decode-time)))
	(mess "")
	)
    ;; DAY
    (setq mess "Day:")
    (while (or (< day 1)
	       (> day 31))
      (setq day (read-number mess))
      (when (or (< day 1) 
		(> day 31));; Incorrect day!
	(setq mess "Please write a number between 1 up to 31. Day:")
	)
      )
    ;; MONTH
    (setq mess "Month:")
    (while (or (< month 1)
	       (> month 12))      
      (setq month (read-number mess))
      (when (or (< month 1)
		(> month 12)) ;; Incorrect month!
	(setq mess "Please write a number between 1 up to 12. Month:")
	)
      )
    ;; YEAR
    (setq mess "Year:")
    (while (or (< year 2000)
	       (> year max-year))
      (setq year (read-number mess))
      (when (or (< year 2000)
		(> year max-year))
	(setq mess (concat "Please write a number between 2000 and "
			   (number-to-string max-year)
			   ". Year:"))
	)
      )
    
    ;; HOUR
    (setq mess "Hour:")
    (while (or (< hour 1)
	       (> hour 24))
      (setq hour (read-number mess))
      (when (or (< hour 1)
		(> hour 24))
	(setq mess (concat "Please write a number between 1 and 24. Hours:"))
	)
      )

    ;; MIN
    (setq mess "MIN:")
    (while (or (< min 0)
	       (> min 59))
      (setq min (read-number mess))
      (when (or (< min 0)
		(> min 59))
	(setq mess (concat "Please write a number between 0 and 59. Minutes:"))
	)

      )
    
    (encode-time sec min hour day month year)
    )
  )

					; Streams!

(defun diaspora-get-participate-stream ()
  "Show the participate stream."
  (interactive)
  (diaspora-get-stream-by-name diaspora-participate-stream-name))  

(defun diaspora-get-entry-stream (&optional max-date)
  "Show the entry stream. 
First look for the JSON file at `diaspora-entry-stream-name' and then parse it.
I expect to be already logged in. Use `diaspora' for log-in.

MAX-DATE is an optional parameter that defines the date interval of the post you want to fetch. 
This parameters has the same format as `current-time' but with the third parameter ignored(or absent): 
  (HIGH LOW MICROSECOND)  
Where HIGH are the 16 bits most significant bit values and LOW are the 16 bits least significant bit values. 
MICROSECOND are ignored, even can be absent."
  (interactive)  
  (diaspora-get-stream-by-name diaspora-entry-stream-name max-date)
  )

(defun diaspora-get-entry-stream-up-to-date ()
  "Read the max date from the user and show the stream.

In other words, look for posts up to that date."
  (interactive)
  (let ((max-date (diaspora-read-date)))
    (diaspora-get-entry-stream max-date)
    )
  )

(defun diaspora-one-day-more (from-date)
  "Adds one day more at the FROM-DATE. 
FROM-DATE must be in the format like `current-date' returns:
  (HIGH LOW MICROSECOND)
Where HIGH are the 16 bits most significant bit values and LOW are the 16 bits least significant bit values. 
MICROSECOND are ignored, even can be absent.

The return value is in the same format as the FROM-DATE parameter."
  (seconds-to-time
   (+ 86400 (float-time from-date))) ;; one day in seconds is 86400: add one day in seconds.   
  )

(defun diaspora-get-entry-stream-next-oldies ()
  "Get the next olds post of the entry stream.

I use the `diaspora-stream-last-post-date' variable.

This is the same as going up to the bottom of the page and let diaspora reload the older posts."
  (interactive)
  (if diaspora-stream-last-post-date
      (progn
	(diaspora-get-entry-stream diaspora-stream-last-post-date)
	)
    (message "You need to get the a stream: there is no last post!")
    )
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

(defvar diaspora-show-message-map-stream
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cc" 'diaspora-comment-message)
    (define-key map [return] 'diaspora-show-message-new-buffer)
    (define-key map [mouse-2] 'diaspora-show-message-new-buffer)
    map)
  "Keymap used when the user clics on a name link.")

(defvar diaspora-like-message-map-stream
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cc" 'diaspora-comment-message)
    (define-key map [return] 'diaspora-like-message)
    (define-key map [mouse-2] 'diaspora-like-message)
    map)
  "Keymap used when the user clics on a name link.")

(defvar diaspora-stream-message-map
  (let ((map (make-sparse-keymap)))
    (define-key map [\C-q] 'diaspora-single-message-destroy)
    map)
  "Keymap used in the stream and messages buffers.")

(defvar diaspora-comment-message-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'diaspora-comment-message)
    (define-key map [mouse-2] 'diaspora-comment-message)
    map)
  "Keymap used in the stream and messages buffers for commenting a message.")

(defvar diaspora-reshare-message-map-stream
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cc" 'diaspora-comment-message)
    (define-key map [return] 'diaspora-reshare-message)
    (define-key map [mouse-2] 'diaspora-reshare-message)
    map)
  "Keymap used when the user clics on a reshare link.")

(defvar diaspora-show-source-code-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'diaspora-show-markdown)
    (define-key map [mouse-2] 'diaspora-show-markdown)
    map)
  "Keymap used when the user clics on a source link.")

(defvar diaspora-hide-source-code-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'diaspora-hide-markdown)
    (define-key map [mouse-2] 'diaspora-hide-markdown)
    map)
  "Keymap used when the user clics on a source link.")


(defvar diaspora-next-oldies-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'diaspora-get-next-oldies)
    (define-key map [mouse-2] 'diaspora-get-next-oldies)
    map)
  "Keymap used in the stream and messages buffers for commenting a message.")


;; A few notes about the next functiom `diaspora-show-message`
;; date: 20120128
;;
;; It would be much easier not to insert the text with properties as is done
;; I think it is preferable to add the properties latter on; using the same type
;; of procedures that is used to insert images. Just a thought.

(defun diaspora-show-like (parsed-like)
  "Write only one likes.

PARSED-LIKE is a JSON part of the likes array.

Modify this function if you want to show more information or show it in other way."
  (let ((author (cdr (assoc 'name (assoc 'author parsed-like))))
	(username (cdr (assoc 'diaspora_id (assoc 'author parsed-like))))
	)
    (insert (diaspora-add-link-to-userstream author (diaspora-get-username username)) "|")
    )
  )

(defun diaspora-show-all-likes (all-parsed-likes)
  "Write in the current buffer the people who likes this post."
  (if (arrayp all-parsed-likes)
      (let ((le (length all-parsed-likes)) ;; is an array
	    )
	(dotimes (i le)
	  (diaspora-show-like (aref all-parsed-likes i))
	  )
	)
    (dolist (elt all-parsed-likes) ;; Is a list
      (diaspora-show-like elt)
      )
    )
  )

(defun diaspora-show-message (parsed-message &optional buffer show-last-three-comments)
  "Show a parsed message in a given buffer.
If buffer is nil, then use the `current-buffer'."
  ;; Ensure that buffer is not nil, in case is nil, buffer will be `current-buffer'.
;; debug
;  (setq aux  parsed-message)
  (let ((buffer (if (null buffer)
		    (current-buffer)
		  buffer)))
    (with-current-buffer buffer
      (let* ((begin-message (point))
	     (id (cdr (assoc 'id parsed-message)))
	     (guid (cdr (assoc 'guid parsed-message)))
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
			       '(interactions comments_count) parsed-message))
	     (amount-likes (diaspora-extract-json-list
			    '(interactions likes_count) parsed-message))
	     (amount-reshares (diaspora-extract-json-list
			       '(interactions reshares_count) parsed-message))
	     (likes (diaspora-extract-json-list
		     '(interactions likes) parsed-message))
	     (public (cdr (assoc 'public parsed-message)))
	     (provider-name (cdr (assoc 'provider_display_name parsed-message)))
	     (post-type (diaspora-extract-json 'post_type parsed-message))
	     )
	
	(insert (concat
		 (propertize 
		  "          ====================          \n"
		  'diaspora-message-separator t)))
	(insert "![" name "](" avatar ")\n")
	(insert (propertize
		 (format "%s (%s):" name diaspora_id)
		 'diaspora-is-user-name t)
		 "\n")
	(insert (format "%s\n" (diaspora-parse-timestamp date)))
	(insert (propertize
		 (format "Has %s comments. %s likes. %s reshares." amount-comments amount-likes amount-reshares)
		 'diaspora-is-amount-comments t)
		"\n")
	(when likes 
	  (insert "Who likes this:\n")
	  (diaspora-show-all-likes likes)
	  (insert "\n")
	  )
	(insert (diaspora-add-comment-link "Comment" id guid)
		" | "	 
		(diaspora-add-link-to-publication "Read in new buffer" id guid)
		" | "
		(diaspora-add-like-link "I like it!" id guid)
		" | "
		(diaspora-add-reshare-link "Reshare" id guid)
		"\n")
	(when (string= "Reshare" post-type)
	  (diaspora-insert-reshare-data parsed-message)
	  )

	(insert (format "%s\n\n" text))

	(if (equal (length photos) 0) ""
	  (diaspora-insert-photos-markdown photos))	
	(if (equal public t)
	    (insert "Public")
	  (insert "Limited")
	  )

	(if provider-name
	    (insert (format " - Published using *%s*\n" provider-name))
	  (insert " - Published using web(or there's no provider name!)\n")
	  )	

	(insert (diaspora-add-source-link "Source code" id guid) 
		"|"
		(diaspora-add-hide-source-link "Hide Source code" id guid))
	  
	(when show-last-three-comments
	  (insert  "\n"
		   (propertize 
		    "Comments:"
		    'diaspora-comments-start t)
		   "\n")
	  (diaspora-comments-show-last-three parsed-message)
	  (insert "\n")	  
	  )
	
	;; At the end of the message: set all the message the diaspora-id-message property
	(add-text-properties begin-message (point) 
			     (list 
			      'diaspora-id-message id
			      'diaspora-guid-message guid))
	)    
      )    
    )
  )

(defun diaspora-insert-reshare-data (parsed-message)
  "Look for reshare data in PARSED-MESSAGE and insert it in the current buffer."
  (let ((name (diaspora-extract-json-list '(root author name) parsed-message))
	(author-id (diaspora-extract-json-list '(root author diaspora_id) parsed-message))
	(post-id (diaspora-extract-json-list '(root id) parsed-message))
	(guid (diaspora-extract-json-list '(root guid) parsed-message))
	)
    (insert "Reshare from: ")
    (insert (propertize
	     (format "%s (%s):" name author-id)
	     'diaspora-is-user-name t)
	    "\n") 
    (insert (diaspora-add-link-to-publication "Read Original" post-id guid)
	    "\n\n")
    )  
  )  

(defun diaspora-insert-photos-markdown (photos &optional buffer)
  "Insert photos in markdown format.

PHOTOS may be an array or just an element of a JSON parsed message.

For some reason Diaspora return two tipes of photos fields in the JSON message:
 * One `json-read' returns it as an array.
 * The other `json-read' returns it as a list.
This parses the two options!"
  (cond 
   ((arrayp photos) ;; is a stream message JSON photo field!
    (let ((le (length photos)) ;; is an array... is a different entry!
	  (i 0))
      (dotimes (i le)	  
	(insert "![photo](" 
		(cdr (assoc 'large (assoc 'sizes (aref photos i))))
		")\n")))
    )
   ((listp photos) ;; Is a single message JSON photo field!
    (dolist (photo photos)
      (insert "![photo](" 
	      (or (cdr (assoc 'large (assoc 'sizes (car photo))))
		  (cdr (assoc 'large (assoc 'sizes photo))))
	      ")\n"))
    )
   )
  )

(defun diaspora-add-source-link (text id-message guid-message)
    "Return a propertized text with a link to publication. Ready to use with a map like `diaspora-show-message-map'
or a function like `diaspora-show-message-new-buffer'."
    (propertize
     text
     'mouse-face 'diaspora-mouse-highlight-face
     'face "link"
     'keymap diaspora-show-source-code-map
     'diaspora-is-link-to-pub t
     'diaspora-id-message id-message
     'diaspora-guid-message guid-message
     'help-echo "Click here for show the source code.")
  )

(defun diaspora-add-hide-source-link (text id-message guid-message)
    "Return a propertized text with a link to publication. Ready to use with a map like `diaspora-show-message-map'
or a function like `diaspora-show-message-new-buffer'."
    (propertize
     text
     'mouse-face 'diaspora-mouse-highlight-face
     'face "link"
     'keymap diaspora-hide-source-code-map
     'diaspora-is-link-to-pub t
     'diaspora-id-message id-message
     'diaspora-guid-message guid-message
     'help-echo "Click here for hide the source code.")
  )



(defun diaspora-add-comment-link (text id-message guid-message)
    "Return a propertized text with a link to publication. Ready to use with a map like `diaspora-show-message-map'
or a function like `diaspora-show-message-new-buffer'."
    (propertize
     text
     'mouse-face 'diaspora-mouse-highlight-face
     'face "link"
     'keymap diaspora-comment-message-map
     'diaspora-is-link-to-pub t
     'diaspora-id-message id-message
     'diaspora-guid-message guid-message
     'help-echo "Click here to comment this message in new buffer.")
  )

(defun diaspora-add-link-to-publication (text id-message guid-message)
  "Return a propertized text with a link to publication. Ready to use with a map like `diaspora-show-message-map'
or a function like `diaspora-show-message-new-buffer'."
  (propertize
   text
   'mouse-face 'diaspora-mouse-highlight-face
   'face "link"
   'keymap diaspora-show-message-map-stream
   'diaspora-id-message id-message
   'diaspora-guid-message guid-message
   'diaspora-is-link-to-pub t
   'help-echo "Click here to see this message in new buffer.")
  )
(defun diaspora-add-like-link (text id-message guid-message)
  "Return a propertized text with a link for sending a \"like\". Ready to use with a map like `diaspora-like-message-map-stream'."
  (propertize
   text
   'mouse-face 'diaspora-mouse-highlight-face
   'face "link"
   'keymap diaspora-like-message-map-stream
   'diaspora-id-message id-message
   'diaspora-guid-message guid-message
   'diaspora-is-like-link t
   'help-echo "Click here to declare that I like this post!")  
  )

(defun diaspora-add-reshare-link (text id-message guid-message)
  "Return a propertized text with a link for sending a \"reshare\". Ready to use with a map like `diaspora-like-message-map-stream'."
  (propertize
   text
   'mouse-face 'diaspora-mouse-highlight-face
   'face "link"
   'keymap diaspora-reshare-message-map-stream
   'diaspora-id-message id-message
   'diaspora-guid-message guid-message
   'diaspora-is-like-link t
   'help-echo "Click here to declare that I like to *reshare* this post!")  
  )

(defun diaspora-add-next-oldies-link (text)
  "Return a propertized text with a link for getting the next oldies posts."
  (propertize
   text
   'mouse-face 'diaspora-mouse-highlight-face
   'face "link"
   'keymap diaspora-next-oldies-map
   'diaspora-is-link-to-pub t
   'help-echo "Click here to get the next oldies!")
  )

(defun diaspora-get-id-message-near-point ()
  "Get the diaspora-id-message property value searching from point.
Use it for getting the nearest id post number when selecting a message."
  ;; Think is no more needed now that the id message is everywhere!
  ;; (get-text-property (+ 1 (previous-single-property-change (+ (point) 1) 'diaspora-id-message))
  ;; 		     'diaspora-id-message))
  (get-text-property (point) 'diaspora-id-message))
  

(defun diaspora-get-guid-message-near-point ()
  "Get the diaspora-guid-message property value searching from point.
Use it for getting the nearest id post number when selecting a message."
  (get-text-property (+ 1 (previous-single-property-change (+ (point) 1) 'diaspora-guid-message))
		     'diaspora-guid-message))

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
  "Get from the `diaspora-post-url' URL the given message by id."
  (window-configuration-to-register diaspora-single-message-register)
  (let ((buff (get-buffer-create diaspora-single-message-buffer))
	(buff-http (diaspora-get-url-entry-stream
		    (diaspora-post-url id-message "json"))))
    (unless buff-http
      (error "I (`diaspora-get-single-message') couldn't retrieve any useful information!")
      )
    (with-current-buffer buff-http
      ;; Delete HTTP header!
      (diaspora-delete-http-header))
    (let ((inhibit-read-only t))
      (diaspora-parse-single-message-json buff-http buff nil)
      (diaspora-insert-comments-for-message id-message buff)
      )
    (diaspora-kill-buffer-safe buff-http)
    
    (switch-to-buffer-other-window buff)
;    (switch-to-buffer buff)
    (with-current-buffer buff
      (diaspora-mode)
      (diaspora-stream-mode)
      (diaspora-remove-bad-chars)
      (diaspora-replace-bad-links)
      (diaspora-hide-markdown)
      )
    )
  )

(defun diaspora-parse-single-message-json (buff-from buff-to &optional show-last-three-comments)
  "Parse JSON format of a single message from buffer \"buff-from\" and return into \"buff-to\""
  (with-current-buffer buff-from
    ;; Get the post message parsed from JSON
    (goto-char (point-min))
    (let ((json-array-type 'list)
	  (json-object-type 'alist))
      (condition-case nil
	  (let ((lstparsed  (json-read)))
	    (with-current-buffer buff-to
	      ;; Clean buffer buff-to and insert message
	      (delete-region (point-min) (point-max))
	      (diaspora-show-message lstparsed nil show-last-three-comments)))
	(message "I (`diaspora-parse-single-message-json') couldn't parse the JSON file downloaded from Diáspora. Internet is fine?")
	)
      )
    )
  )
  

(defun diaspora-parse-json (buffer-from buffer-to &optional status)
  "Parse de JSON entry stream.
Take the JSON format, read it from buffer-from and write into buffer-to buffer.

Also save the last post date for getting the next posts(older posts) in the stream using `diaspora-get-entry-stream-next-oldies'."
  (with-current-buffer buffer-from
    (goto-char (point-min))
    (window-configuration-to-register diaspora-stream-register)
    ;; Create a new buffer called according `diaspora-buffer' say 
    ;; and parse the json code into lists.
    (let* (;;(json-array-type 'list)
	   ;;(json-object-type 'alist)       
	   (parsed-json (json-read))
	   (lstparsed parsed-json)
	   )      
      ;; Save the last post's date
      (setq diaspora-stream-last-post-date (diaspora-get-last-post-time parsed-json))
      ;; clean the new buffer
      (with-current-buffer buffer-to
	(let ((le (length lstparsed))
	      (inhibit-read-only t))
	  (delete-region (point-min) (point-max))
	  ;; Show all elements
	  (dotimes (i le)
	    (diaspora-show-message (aref lstparsed i) buffer-to t))
	  (insert (diaspora-add-next-oldies-link "Next Oldies Posts ->"))))
      )
    )
  )

;; images: needs working

(defun diaspora-get-user-avatar (url &optional user-id)
  (let ((url-request-method "GET")
	(url-show-status nil)	
	)
    (diaspora-debug-msg "***GETing:")
    (diaspora-debug-msg url)

    (url-retrieve url 'diaspora-write-image
		  (list url user-id)))
  )
				 
(defun diaspora-get-image (url &optional function)
  "Retrieve the image asynchronously and call the given function.
FUNCTION must recieve two parameters:
The status and the url. See `url-retrieve'."
  (let ((url-request-method "GET")
	(url-show-status nil))

    (diaspora-debug-msg "***GETing:")
    (diaspora-debug-msg url)

    (url-retrieve url '(lambda (status url function)
			 (diaspora-write-image status url) ; write into a file! then call the function...
			 (funcall function status url))
		  (list url function))))

(defun diaspora-get-image-sync (url)
  "Same as `diaspora-get-image' but synchronously."
  (let ((url-request-method "GET")
	(url-show-status nil))

    (diaspora-debug-msg "***GETing:")
    (diaspora-debug-msg url)

    (with-current-buffer (url-retrieve-synchronously url)
      (diaspora-write-image nil url)
      (diaspora-kill-buffer-safe)
      )
    )
  )


(defun diaspora-write-image (status url &optional user-id)
  (let ((image-file-name
	 (diaspora-image-path-from-url url user-id))
	(end-image (search-forward "\C-j\C-j" nil t))
	)
    (setq buffer-file-coding-system 'no-conversion)
    (setq buffer-file-name image-file-name)
    (goto-char (point-min))
    (when end-image
      (delete-region (point-min) (search-forward "\C-j\C-j" nil t))
      )
    (save-buffer 0)
    (diaspora-kill-buffer-safe (current-buffer)))
  )

(defun diaspora-get-all-images ()
  (interactive)
  (mapcar 'diaspora-get-image-if-necessary (diaspora-get-all-image-links)))

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
  (condition-case nil
      (add-text-properties (cadr ipoint) (cddr ipoint)
			   (list 'display (create-image (diaspora-image-path-from-url (car ipoint))))
			   )
    (error nil)) ;; Don't throw errors... creating an image that emacs doesn't understand creates an error.
  )

(defun diaspora-get-and-show-images (&rest r)
  "Look for all images URL and download all of them into the temporary directory, then insert them in the current buffer where those URLs are."
  (interactive)
  (let ((inhibit-read-only t))    
    (diaspora-get-all-images)
    (diaspora-show-images)
    )
  )
  

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
  (save-excursion
    (cond ((search-forward-regexp regexp (point-max) t)
	   (cons (cons (match-string-no-properties 
			(if (not opt) 2
			  opt))
		       (cons (match-beginning 0) 
			     (match-end 0)))
		 (diaspora-get-all-regexp-markdown-points regexp
						  (if (not opt) 2
						    opt))))
	  (t nil))))

(defvar diaspora-user-image-dir ""
  ""
  )

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



(defun diaspora-find-image-links ()
  "Search for all strings that matchs `diaspora-regexp-image' from point until the end, in other words: search for all links from here."  
  (cond ((search-forward-regexp diaspora-regexp-image (point-max) t)
	 (cons (match-string-no-properties 2)
	       (diaspora-find-image-links)))
	(t nil)
	) 
  )

(defun diaspora-get-all-image-links ()
  "Return all image links in the current buffer.
Image links must match the regexp in `diaspora-regexp-image'."
  (goto-char (point-min))
  (save-excursion
    (remove-duplicates (diaspora-find-image-links) :test 'equal)
    )
  )

(defun diaspora-get-image-link-at-point ()
  "Get the image near the point"
  (save-excursion
    (goto-char (point-at-bol))
    (when (search-forward-regexp diaspora-regexp-image nil t)
      (match-string-no-properties 2))))  

(defun diaspora-see-regexp-markdown ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((markdown-points (diaspora-get-all-regexp-markdown-points  diaspora-regexp-tag 0)))
      (dolist (mpoint markdown-points)
	(add-text-properties (cadr mpoint) (cddr mpoint)
			     (list 'mouse-face 'diaspora-mouse-highlight-face
				   'face "link"
				   'keymap diaspora-show-tag-map
				   'diaspora-tag (car mpoint)
				   'help-echo "Click here to see the tag stream in new buffer."))
	))))

    ;; (goto-char (point-min))
    ;; (let ((markdown-points (diaspora-get-all-regexp-markdown-points  diaspora-regexp-user-entry 0)))
    ;;   (dolist (mpoint markdown-points)
    ;; 	(add-text-properties (cadr mpoint) (cddr mpoint)
    ;; 			     (list 'mouse-face 'diaspora-mouse-highlight-face
    ;; 				   'face "link"
    ;; 				   'keymap diaspora-show-message-map
    ;; 				   'diaspora-id-message id
    ;; 				   'help-echo "Click here to see this message in new buffer."))
    ;; 	))))
    
;; (insert (propertize
	;; 	 (format "%s(%s):\n" name diaspora_id)
	;; 	 'mouse-face 'diaspora-mouse-highlight-face
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
  "Extract the value from the E key in the JSON parsed list A."
  (cdr (assoc e a)))

(defun diaspora-extract-json-list (e a)
  "Extract a value from a JSON parsed list A using a list of hierarchical keys.
For example:
 (diaspora-extract-json-list '(a b c) json-list)

Will look for A, then B in A and then C in B in A and return C's values.
"
  (cond (e
	 (diaspora-extract-json-list (cdr e) 
			 (diaspora-extract-json (car e) a)))
	(a)))

(defun diaspora-get-stream-by-tag (tag)
  "Get a stream of the messages with the tag given by 'tag'.
The tag must be a string without the starting \"#\"."
  (interactive "MTag(without '#')?")
  (diaspora-get-stream-by-name (format "/tags/%s" tag)))

(defun diaspora-tag-stream ()
  "Get the stream by the tag #diaspora-el so you can see the latest news of diaspora.el!"
  (interactive)
  (diaspora-get-stream-by-tag diaspora-tag)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun diaspora-get-url(url)
  "Get a diaspora URL and leave it in a new buffer."
  (let ((url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")
	   ("Accept-Language" . "en")
	   ("Accept-Charset" . "utf-8")))
	(buffer-file-coding-system 'utf-8)
	)

    (diaspora-debug-msg "***GETing:")
    (diaspora-debug-msg url)

    (url-retrieve-synchronously url)))


(defun diaspora-json-read-url (url)
  "Returns a JSON parsed string from URL."
  (interactive)
  (let ((json-array-type 'list)
	(json-object-type 'alist)
	(http-buffer (diaspora-get-url url)))
    (with-current-buffer http-buffer
      (diaspora-delete-http-header)
      (condition-case e-var
	  (let ((stream-parsed (json-read)))
	    (diaspora-kill-buffer-safe http-buffer)
	    stream-parsed)
	((json-readtable-error end-of-file)
	 (message "I (`diaspora-json-read-url') coudn't parse the JSON! Not connected? Problems with URLs? Not logged in? Wrong username or password?")
	 )
	)
      )
    )
  )


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

(defun diaspora-get-image-if-necessary (url &optional get-anyway)
  "If it hasn'd downloaded, download the image and save it in the temp directory.

If GET-ANYWAY is setted, download the image without any checks."
  (let ((image-name (file-name-nondirectory url)))
    (if (or (not (file-exists-p (diaspora-image-path image-name)))
	    get-anyway)
      (diaspora-get-image-sync url))
    (diaspora-image-path image-name)))

(defun diaspora-get-image-if-necessary-async (url &optional function)
  "If it hasn'd downloaded, download the image asynchronously and save it in the temp directory.
After that, call the given FUNCTION."
  (let ((image-name (file-name-nondirectory url)))
    (if (file-exists-p (diaspora-image-path image-name))
	(diaspora-image-path image-name)
      (diaspora-get-image url function)
      )
    )
  )
      

(defun diaspora-show-image-at-region ()
  "Consider the region as the image's URL, download it(if necessary) and open an external program to see it."
  (interactive)
  (let ((url (buffer-substring-no-properties (region-beginning) (region-end))))
    (diaspora-get-image-if-necessary url)
    (diaspora-open-image-program (diaspora-image-path (file-name-nondirectory url)))
    )
  )

(defun diaspora-show-image-at-point (&optional get-anyway)
  "Show only the image at the cursor.

If the image already exists, it won't download again, except if GET-ANYWAY is setted to t."
  (interactive "P")
  (when get-anyway
    (setq get-anyway t))
  (let ((image-url (diaspora-get-image-link-at-point)))
    (diaspora-get-image-if-necessary image-url get-anyway)
    (diaspora-open-image-program (diaspora-image-path (file-name-nondirectory image-url)))))

(defun diaspora-callback-for-opening-image-from-url (status url)
  "This is a callback for `diaspora-get-image'.
Its open the image viewer with the image taken from the url."
  (diaspora-open-image-program (diaspora-image-path (file-name-nondirectory url)))
  )

(defun diaspora-get-and-show-image-async (url &optional get-anyway)
  "Get the URL if necessary(if not downloaded before) and show it with an external viewer(using `diaspora-image-external-program')."
  (let ((image-name (file-name-nondirectory url)))
    (if (and (file-exists-p (diaspora-image-path image-name))
	     (not get-anyway))
	(diaspora-callback-for-opening-image-from-url nil url)
      (diaspora-get-image url 'diaspora-callback-for-opening-image-from-url)
      )
    )
  )  
  
(defun diaspora-refresh-image-at-point (&rest r)
  "Download the image again so emacs can refresh it."
  (interactive)
  (let ((image-url (diaspora-get-image-link-at-point)))
    (diaspora-get-image-if-necessary image-url t)
    )
  )
  

(defun diaspora-show-image-at-point-async (&optional get-anyway)
  "Show only the image at the cursor."
  (interactive "P")
  (when get-anyway
    (setq get-anyway t))
  (let ((image-url (diaspora-get-image-link-at-point)))
    (diaspora-get-and-show-image-async image-url get-anyway)
    )
  )

(defun diaspora-open-image-program (image-path)
  (let ((command-string (concat
			 diaspora-image-external-program
			 " "
			 image-path)))
    (async-shell-command command-string)))

					; ********************
					; Authenticity token functions

(defun diaspora-get-authenticity-token-if-necessary (&optional url get-anyway)
  "Check `diaspora-get-always-authenticity-token', and if it's true get the authenticity token. 
If false, check if there is an authenticity token saved, if not get it.

If URL is a string then get from this URL instead from (`diaspora-url' `diaspora-sign-in-url')(sing in URL).
If GET-ANYWAY is t then get it from Internet despite everything.
"
  (when (or get-anyway
	    diaspora-get-always-authenticity-token 
	    (null diaspora-auth-token))
    ;; Get the authenticity token    
    (if url
	(diaspora-authenticity-token url)
      (diaspora-authenticity-token (diaspora-url diaspora-sign-in-url)) 
      )
    (message "Diaspora: Authenticity token obtained")) 
  )

(defun diaspora-get-last-post-time (stream-json-parsed)
  "Return the last post time so you can use it for fetching the next part of the streams with older posts.
The return value is a list in the format like `current-time' or `encode-date'(in fact I use that function):
  (HIGH LOW MICROSECOND)
Where HIGH are the 16 bits most significant bit values and LOW are the 16 bits least significant bit values. 
MICROSECOND are ignored, even can be absent.

STREAM-JSON-PARSED is the stream in JSON format parsed with `json-read'."
  (let* (
	 (post-arr stream-json-parsed) ;; return the posts array
	 (last-post (aref post-arr (1- (length post-arr)))) ;; return the last post
	 (interacted-date (cdr (assoc 'created_at last-post))) ;; return the string with the last created_at date
	 (year (string-to-number (substring interacted-date 0 4)))
	 (month (string-to-number (substring interacted-date 5 7)))
	 (day (string-to-number (substring interacted-date 8 10)))
	 (hour (string-to-number (substring interacted-date 11 13)))
	 (min (string-to-number (substring interacted-date 14 16)))
	 (sec (string-to-number (substring interacted-date 17 19)))
	 )
    (encode-time sec min hour day month year)
    )
  )



(defun diaspora-like-message (&rest r)
  "Send a \"like\" for this message!"
  (interactive)
  (diaspora-send-likes (diaspora-get-id-message-near-point))
  )

(defun diaspora-send-likes (post-id)
  "Send a like POST for the message with id given by POST-ID."
  (when post-id
    (let ((url-request-method "POST")
	  (url-request-extra-headers
	   '(("Content-Type" . "application/x-www-form-urlencoded")
	     ("Accept-Language" . "en")
	     ("Accept-Charset" . "utf-8")))
	  (buffer-file-coding-system 'utf-8)
	  (url-request-data  ;; there is no need of information
	   (mapconcat (lambda (arg)
			(concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		      (list (cons "user[username]" diaspora-username)
			    (cons "user[password]" diaspora-password)
			    (cons "user[remember_me]" "1")
			    (cons "authenticity_token" diaspora-auth-token))
		      "&")))

      (diaspora-debug-msg "***POSTing:")
      (diaspora-debug-msg (diaspora-likes-url post-id))
      (diaspora-debug-msg url-request-data)

      (url-retrieve-synchronously (diaspora-likes-url post-id))
      (diaspora-stream-check-http-error)
      ;;(diaspora-kill-buffer-safe)
      )
    )
  )

(defun diaspora-reshare-message (&rest r)
  "Send a \"reshare\" for this message!"
  (interactive)
  (diaspora-send-reshare (diaspora-get-guid-message-near-point))
  )

(defun diaspora-send-reshare (post-guid)
  "Send a reshare POST"
  (when post-guid
    (let ((url-request-method "POST")
	  (url-request-extra-headers
	   '(("Content-Type" . "application/x-www-form-urlencoded")
	     ("Accept-Language" . "en")
	     ("Accept-Charset" . "utf-8")))
	  (buffer-file-coding-system 'utf-8)
	  (url-request-data  ;; there is no need of information
	   (mapconcat (lambda (arg)
			(concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		      (list (cons "user[username]" diaspora-username)
			    (cons "user[password]" diaspora-password)
			    (cons "user[remember_me]" "1")
			    (cons "authenticity_token" diaspora-auth-token)
			    (cons "root_guid" post-guid))
			    "&"))
	   )

      (diaspora-debug-msg "***POSTing:")
      (diaspora-debug-msg (diaspora-reshare-url))
      (diaspora-debug-msg url-request-data)

      (url-retrieve-synchronously (diaspora-reshare-url))
      ;;(diaspora-kill-buffer-safe)
      )
    )
  )
  

(defun diaspora-stream-reset ()
  "Reset all this library as if diaspora has just started."
  (setq diaspora-stream-last-post-date nil)
  (setq diaspora-last-stream-visited nil)
  )


(defun diaspora-parse-timestamp (date)
  "Parse the DATE and time from the format it recieve in JSON(like '2013-01-10T11:34:27Z') to 
a list '(date time).
Where date is a list with (year month day) and time is a list with (hour minutes seconds).

Remember: this time is in UTC-0. So it needs to be changed to your timezone if you want to print it correctly. 
Fortunately we'll use it for calculate how much time has passed... :)"
  (let* (
	 (utc-date (date-to-time date))
	 (seconds (float-time utc-date)) ;; Seconds from epoch of DATE
	 (cur-date (current-time))
	 (cur-seconds (float-time cur-date)) ;; Seconds from epoch of `current-time'
	 (difference (/ (- cur-seconds seconds) 60)) ;; Difference is in minutes
	 )
    (cond ((<= difference 1)
	   "Less than a minute ago")
	  ((<= difference 60) ;; Less than an hour
	   (format "%s minutes ago"
		   (floor difference)))
	  ((<= difference 1440) ;; Less than a day
	   (format "%s hours ago"
		   (floor (/ difference 60))))
	  ((<= difference 44640) ;; Less than a month
	   (format "%s days ago"
		   (floor (/(/ difference 60) ; in hours
			    24)))) ; in days
	  ;;((<= difference 518400)
	  (t
	   (format "%s months ago ( %s )"
		   (floor
		    (/ (/ (/ difference 60) ; in hours
			  24) ; in days
		       30)) ; in months aprox. (30 days) 
		   date
		   )
	   )
	  )
    )  
  )

(provide 'diaspora-stream)
