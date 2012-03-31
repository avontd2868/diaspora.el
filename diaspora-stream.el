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

					; ********************
					; Customization

(defgroup diaspora-streams nil
  "URL and names for the Streams used in diaspora.el."
  :group 'diaspora
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

(defcustom diaspora-entry-stream-url 
  "/stream"
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

(defcustom diaspora-entry-likes-url 
  "https://joindiaspora.com/participate.json"
  "JSON version of the entry stream(the main stream)."
  :group 'diaspora)


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
    (kill-buffer buf-kill)))

(defun diaspora-get-time-by-timezone (max-time)
  "Return the time in seconds from the epoch modified according to the timezone specified by `diaspora-timezone' to represents the time
in the UTC standard.

This is usefull for giving this as a GET(or POST) \"max_time\" parameter for any stream."
  (+ (float-time max-time) (* diaspora-timezone 3600)) ;; 3600 is one hour.
  )


(defun diaspora-get-url-entry-stream (url &optional max-time)
  "Get the Diáspora URL and leave it in a new buffer.
Returns: A new buffer where is all the information retrieved from the URL."
  (let ((url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")
	   ("Accept-Language" . "en")
	   ("Accept-Charset" . "utf-8")))
	(buffer-file-coding-system 'utf-8))
    (if max-time
	
	(let ((url-request-data ;; the interval of time has been setted
	       (mapconcat (lambda (arg)
			    (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
			  (list (cons "max_time" (number-to-string (diaspora-get-time-by-timezone max-time))))
				"&"))
	      )
	  (url-retrieve-synchronously url)) 
      
      (url-retrieve-synchronously url);; there is no interval of time
      
      )
    )
  )

(defun diaspora-delete-http-header ()
  "Delete the first lines that is the HTTP header in the current buffer.
This is used after getting a stream or any URL in JSON format."
   (goto-char (point-min))
   (search-forward "\n\n")      
   (delete-region (point-min) (match-beginning 0)))

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

(defun diaspora-get-stream(stream-url max-time)
  "Get the stream given by the url, and then, show it in the diaspora buffer.
I expect to be logged in, but if not, I download the authenticity token.

Set MAX-TIME with a valid emacs timestamp to fetch information from and until that interval of time."  
  (diaspora-ask) ;; don't forget username and password!
  (diaspora-get-authenticity-token-if-necessary)
  ;; get the in JSON format all the data
  (let (
	(stream-buff (get-buffer-create diaspora-stream-buffer))
	(buff (diaspora-get-url-entry-stream stream-url max-time )))
    (with-current-buffer buff
      ;; Delete the HTTP header...
      (diaspora-delete-http-header)
      ;; Parse JSON...
      (let ((inhibit-read-only t))
	;; Apply diaspora-mode
	(with-current-buffer stream-buff	  
	  (diaspora-mode) 	
	  )
	(diaspora-parse-json buff stream-buff)
	(if diaspora-show-images-by-default
	    (progn
	      (diaspora-get-all-images)
	      (diaspora-show-images)
	      )
	  )
	(goto-char (point-min))
	(switch-to-buffer stream-buff)
	)
      )
    ;; Delete HTTP Buffer
    ;;(kill-buffer buff)
    ))

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
First look for the JSON file at `diaspora-entry-stream-url' and then parse it.
I expect to be already logged in. Use `diaspora' for log-in.

MAX-DATE is an optional parameter that defines the date interval of the post you want to fetch. 
This parameters has the same format as `current-time' but with the third parameter ignored(or absent): 
  (HIGH LOW MICROSECOND)  
Where HIGH are the 16 bits most significant bit values and LOW are the 16 bits least significant bit values. 
MICROSECOND are ignored, even can be absent."
  (interactive)  
  (diaspora-get-stream-by-name diaspora-entry-stream-url max-date)
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
	(insert (diaspora-add-link-to-publication "Read in new buffer\n" id))
	(insert (format "%s\n\n" text))
	(if (equal (length photos) 0) ""
	  (diaspora-insert-photos-markdown photos))	
	(when show-last-three-comments
	  (insert  "\n*Comments:*\n")
	  (diaspora-comments-show-last-three parsed-message)
	  (insert "\n")
	  )
	)      
      )    
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
		(cdr (assoc 'large (car (aref photos i))))
		")\n")))
    )
   ((listp photos) ;; Is a single message JSON photo field!
    (dolist (photo photos)
      (insert "![photo](" 
	      (cdr (assoc 'large (car photo)))
	      ")\n"))
    )
   )
  )

(defun diaspora-add-link-to-publication (text id-message)
  "Return a propertized text with a link to publication. Ready to use with a map like `diaspora-show-message-map'
or a function like `diaspora-show-message-new-buffer'."
  (propertize
   text
   'mouse-face 'highlight
   'face "link"
   'keymap diaspora-show-message-map-stream
   'diaspora-id-message id-message
   'diaspora-is-link-to-pub t
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
    (let ((inhibit-read-only t))
      (diaspora-parse-single-message-json buff-http buff nil)
      (diaspora-insert-comments-for-message id-message buff)
      )
    (switch-to-buffer-other-window buff)
;    (switch-to-buffer buff)
    (diaspora-mode)))

(defun diaspora-parse-single-message-json (buff-from buff-to &optional show-last-three-comments)
  "Parse JSON format of a single message from buffer \"buff-from\" and return into \"buff-to\""
  (with-current-buffer buff-from
    ;; Get the post message parsed from JSON
    (goto-char (point-min))
    (let ((json-array-type 'list)
	  (json-object-type 'alist))
      (let ((lstparsed  (json-read)))
	(with-current-buffer buff-to
	  ;; Clean buffer buff-to and insert message
	  (delete-region (point-min) (point-max))
	  (diaspora-show-message lstparsed nil show-last-three-comments))))))

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
	   (lstparsed (cdr (assoc 'posts parsed-json)))
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
	    (diaspora-show-message (aref lstparsed i) buffer-to t))))
      )
    )
  )

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

(defun diaspora-get-image-sync (url)
  "Same as `diaspora-get-image' but synchronously."
  (let ((url-request-method "GET")
	(url-show-status nil))
    (with-current-buffer (url-retrieve-synchronously url)
      (diaspora-write-image nil url))))


(defun diaspora-write-image (status url &optional user-id)
  (let ((image-file-name
	 (diaspora-image-path-from-url url user-id)))
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
		       (list 'display (create-image (diaspora-image-path-from-url (car ipoint))))
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



(defun diaspora-get-all-image-links ()
  (goto-char (point-min))
  (save-excursion
    (flet ((d-find-aux ()
		       (cond ((search-forward-regexp diaspora-regexp-image (point-max) t)
			      (cons (match-string-no-properties 2)
				    (d-find-aux)))
			     (t nil))))
      (remove-duplicates (d-find-aux) :test 'equal))))

(defun diaspora-get-image-link-at-point ()
  "Get the image near the point"
  (save-excursion
    (goto-char (point-at-bol))
    (when (search-forward-regexp diaspora-regexp-image nil t)
      (match-string-no-properties 2))))  

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

(defun diaspora-get-image-if-necessary (url)
  "If it hasn'd downloaded, download the image and save it in the temp directory."
  (let ((image-name (file-name-nondirectory url)))
    (unless (file-exists-p (diaspora-image-path image-name))
      (diaspora-get-image-sync url))
    (diaspora-image-path image-name)))

(defun diaspora-show-image-at-region ()
  "Consider the region as the image's URL, download it(if necessary) and open an external program to see it."
  (interactive)
  (let ((url (buffer-substring-no-properties (region-beginning) (region-end))))
    (diaspora-get-image-if-necessary url)
    (diaspora-open-image-program (diaspora-image-path (file-name-nondirectory url)))
    )
  )

(defun diaspora-show-image-at-point ()
  "Show only the image at the cursor."
  (interactive)
  (let ((image-url (diaspora-get-image-link-at-point)))
    (diaspora-get-image-if-necessary image-url)
    (diaspora-open-image-program (diaspora-image-path (file-name-nondirectory image-url)))))
  

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
	 (post-arr (cdr (assoc 'posts stream-json-parsed))) ;; return the posts array
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

(provide 'diaspora-stream)