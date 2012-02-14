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

;; Save all the files in a DIR and add that DIR to `load-path'; 
;; for instance `(add-to-list 'load-path "~/emacs.el/disaspora.el/")' to your .emacs
;; Files: diaspora.el, diaspora-post.el  and diaspora-stream.el 

(require 'url)
(require 'url-http)
(require 'json)
(require 'font-lock)

(require 'diaspora-post)
(require 'diaspora-stream)
(require 'diaspora-notifications)

(defconst diaspora-el-version ".0"
  "This version of diaspora*-el.")

(defgroup diaspora nil 
  "A mode for diaspora* stream view and posting."
  :group 'applications)

(defgroup diaspora-streams nil
  "URL and names for the Streams used in diaspora.el."
  :group 'diaspora
  :version "23.0"
  :tag "diaspora streams urls")

;;; User variable:

(defcustom diaspora-secure-pod
  t
  "If your diaspora pod use https, set this to true.
If only use http, use false."
  :type 'boolean
  :group 'diaspora)

(defcustom diaspora-pod 
  "joindiaspora.com"
  "Your diaspora* pod."
  :type 'string
  :group 'diaspora)

(defcustom diaspora-posts-directory
  "~/.diaspora/posts/"
  "Diaspora* temp dir (abs path)."
  :type 'dir
  :group 'diaspora)


(defcustom diaspora-temp-directory
  "~/.diaspora/temp/"
  "Diaspora* temp dir (abs path)."
  :type 'dir
  :group 'diaspora)

(defcustom diaspora-image-directory
  "~/.diaspora/img/"
  "Diaspora* image dir (abs path)."
  :type 'dir
  :group 'diaspora)

(defcustom diaspora-show-images-by-default
  t
  "Loads images by default at start."
  :type 'boolean
  :group 'diaspora)


(defcustom diaspora-show-user-avatar t
   "Show user images beside each users entry."
   :type 'boolean
   :group 'diaspora)

(defcustom diaspora-mode-hook nil
  "Functions run upon entering `diaspora-mode'."
  :type 'hook
  :options '(flyspell-mode turn-on-auto-fill longlines-mode diaspora-get-all-images diaspora-show-images)
  :group 'diaspora)

(defcustom diaspora-username nil
  "Username to use for connecting to diaspora.
If nil, you will be prompted."
  :type 'string
  :group 'diaspora)

(defcustom diaspora-password nil
  "Password to use for connecting to diaspora.
If nil, you will be prompted."
  :type 'string
  :group 'diaspora)

(defcustom diaspora-sign-in-url 
  "/users/sign_in"
  "URL used to signing in."
  :type 'string
  :group 'diaspora-streams)

(defcustom diaspora-bookmarklet-location
  "/bookmarklet"
  "Location of the bookmarklet. This is used in `diaspora-get-aspects' for searching for the aspects.
A bit complicated but the only way known to get a list of aspects."
  :type 'string
  :group 'diaspora-streams)

(defcustom diaspora-status-messages-url 
  "/status_messages"
  "URL used to update diaspora status messages."
  :type 'string
  :group 'diaspora-streams)

(defcustom diaspora-single-message-url
  "/posts"
  "URL used to get a single message."
  :type 'string
  :group 'diaspora-streams)

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
  "/explore"
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
  
(defcustom diaspora-aspects-stream-name
  "aspects"
  "This is the name (as appear in diaspora/config/routes.rb in the diaspora project) of the aspects stream.
This is the name of the page, for example:
If `diaspora-pod' has the value \"joindiaspora.com\", then,
the JSON page is at the URL:
  https://joindiaspora.com/participate.json

And the `diaspora-participate-stream-name' must be at value \"participate\"."
  :type 'string
  :group 'diaspora-streams)

(defcustom diaspora-comment-name 
  "comments"
  "This is the name of the comments for posting."
  :type 'string
  :group 'diaspora-streams)

(defvar diaspora-notifications-url
  "notifications.json"
  "This is the URL for JSON format notifications.")


(defvar diaspora-aspect-alist nil
  "This is an alist of a pair of aspects:
 ((name of the aspect . id of the aspect) ... )

This variable will get its values using the function `diaspora-get-aspects'.")

(defvar diaspora-aspects-for-post nil
  "This is a list of aspects ids. This list is used for posting, and as soon as the newly posted has been sended
to the pod, the information is discarded for a new post!
This variable is intended to be as parameter for `diaspora-post'. 
You may would like to use `diaspora-add-aspect'.")

(defcustom diaspora-entry-file-dir
  "~/public_html/diaspora.posts/"
  "Directory where to save posts made to diaspora*."
  :group 'diaspora)

;; (defcustom diaspora-data-file
;;   "~/.diaspora"
;;   "Name of the file do save posts made to diaspora*."
;;   :type 'file
;;   :group 'diaspora)

(defcustom diaspora-data-directory
  "~/.diaspora/"
  "Directory where for saving."
  :type 'file
  :group 'diaspora)


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


(defcustom diaspora-stream-register ?R
  "The register in which the window configuration is stored."
  :type 'character
  :group 'diaspora)


(defcustom diaspora-post-register ?R
  "The register in which the window configuration is stored."
  :type 'character
  :group 'diaspora)

(defcustom diaspora-single-message-register ?R
  "The register in which the window configuration is stored."
  :type 'character
  :group 'diaspora)

(defcustom diaspora-save-after-posting t
  "*Non-nil means automatically save after posting."
  :type 'boolean
  :group 'diaspora)

;;; Internal Variables:

(defvar  diaspora-webfinger-list nil
  "")

(defvar diaspora-auth-token nil
  "Authenticity token variable name.")

(defvar  diaspora-resource-descriptor-webfinger-string nil
  "")


(defvar diaspora-stream-buffer "*diaspora stream*"
  "The name of the diaspora stream buffer.")

(defvar diaspora-post-buffer "*diaspora post*"
  "The name of the diaspora post buffer.")

(defvar diaspora-single-message-buffer "*diaspora message*"
  "The name of the diaspora single message buffer.")

(defvar  diaspora-stream-tag-buffer
  "*diaspora stream tag*"
  "The name of the diaspora tag stream buffer.")

(defvar diaspora-notifications-buffer
  "*diaspora notifications*"
    "The name of the diaspora notifications buffer.")


;;; User Functions:

(defun diaspora ()
  "Make all dirs if they don' exist and set `diaspora-username' 
and  `diaspora-password' no matter what.  
To be called interactively instead of `diaspora-ask'"
  (interactive)
  (diaspora-make-dirs)
  (diaspora-ask t))
  
(defun diaspora-make-dirs ()
  "Make all dirs if they don' exist."
  (unless (file-exists-p diaspora-data-directory)    
    (make-directory diaspora-data-directory))
  (unless (file-exists-p diaspora-temp-directory)
    (make-directory diaspora-temp-directory))
  (unless (file-exists-p diaspora-posts-directory)
    (make-directory diaspora-posts-directory))
  (unless (file-exists-p diaspora-image-directory)
    (make-directory diaspora-image-directory)))


(defun diaspora-ask (&optional opt)
  "Ask for username and password if `diaspora-username' 
and  `diaspora-password' has not been setted. `opt' t forces setting."
  (unless (and diaspora-username diaspora-password (null opt))
      ;; Diaspora username and password was not setted.
    (list
     (setq diaspora-username (read-string "username: "
					  diaspora-username
					  nil nil))
     (setq diaspora-password (read-passwd "password: ")))))


;; Font lock

(defgroup diaspora-faces nil
  "Faces used in diaspora Mode"
  :group 'diaspora
  :group 'faces)

;; (defcustom diaspora-regex-bare-link
;;   "http://[a-zA-Z0-9-_\./?=&]*"
;; or "^http://.*"
;;   "Regular expression for a `http://'"
;;   :type 'regexp
;;   :group 'diaspora)

(defcustom diaspora-regexp-youtube-link
  "^\\(http.*://www.youtube.com/watch\\?v=\\)\\([^\)].*\\)"
  "Regular expression for a youtube link"
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-image-alist
  "\\(`?http.://\\|\\[\\[\\|<\\|`\\)?\\([-+./_0-9a-zA-Z]+\\.\\(GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|N[GM]\\|PM\\)\\|SVG\\|TIFF?\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|n[gm]\\|pm\\)\\|svg\\|tiff?\\|x\\(?:[bp]m\\)\\)\\)\\(\\]\\]\\|>\\|'\\)?"
  "Taken from iimage-mode."
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-image
"!\\(\\[[^]]*?\\]\\)(\\(`?http.*:[^\\)?]*\\))"
  "Regular expression for a [text](file) or an image link ![text](file).
Note: this is not correct! Needs more thought to get all images right."
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-user-entry 
"^[a-zA-Z0-9_úùüãâáàéíìõóòñ\s-\.\*\/@]*[a-zA-Z0-9_úùüãâáàéíìõóòñ\s-\.\*\/@]*@[a-zA-Z0-9\s-]*[\.a-zA-Z0-9\s-]*)"
  "Regular expression for user entry."
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-tag
  "#[a-zA-Z0-9_/\.-]+"
  "Regular expression for a tag."
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-header-1
  "^\\(# \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 1"
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-header-2
  "^\\(## \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 2"
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-header-3
  "^\\(### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 3"
  :type 'regexp
  :group 'diaspora)


(defcustom diaspora-regexp-header-4
  "^\\(#### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 4"
  :type 'regexp
  :group 'diaspora)

(defconst diaspora-regexp-code
  "\\(^\\|[^\\]\\)\\(\\(`\\{1,2\\}\\)\\([^ \\]\\|[^ ]\\(.\\|\n[^\n]\\)*?[^ \\]\\)\\3\\)"
  "Regular expression for matching inline code fragments.")


(defconst diaspora-regexp-bold
  "\\(^\\|[^\\]\\)\\(\\([*_]\\{2\\}\\)\\(.\\|\n[^\n]\\)*?[^\\ ]\\3\\)"
  "Regular expression for matching bold text.")

(defconst diaspora-regexp-emph
  "\\(^\\|[^\\]\\)\\(\\([*_]\\)\\([^ \\]\\3\\|[^ ]\\(.\\|\n[^\n]\\)*?[^\\ ]\\3\\)\\)"
  "Regular expression for matching emph text.")

(defconst diaspora-regexp-email
  "<\\(\\sw\\|\\s_\\|\\s.\\)+@\\(\\sw\\|\\s_\\|\\s.\\)+>"
  "Regular expression for matching inline email addresses.")

(defconst diaspora-regexp-blockquote
  "^>.*$"
  "Regular expression for matching blockquote lines.")

(defconst diaspora-regexp-hr
  "^\\(\\*[ ]?\\*[ ]?\\*[ ]?[\\* ]*\\|-[ ]?-[ ]?-[--- ]*\\)$"
  "Regular expression for matching markdown horizontal rules.")

(defvar diaspora-header-face-1 'diaspora-header-face-1
  "Face name to use for level-1 headers.")

(defvar diaspora-header-face-2 'diaspora-header-face-2
  "Face name to use for level-2 headers.")

(defvar diaspora-header-face-3 'diaspora-header-face-3
  "Face name to use for level-3 headers.")

(defvar diaspora-header-face-4 'diaspora-header-face-4
  "Face name to use for level-4 headers.")

(defvar diaspora-url-face 'diaspora-url-face
  "Face name to use for URLs.")

(defvar diaspora-link-face 'diaspora-link-face
  "Face name to use for links.")

(defvar diaspora-emph-face 'diaspora-emph-face
  "Face name to use for links.")

(defvar diaspora-bold-face 'diaspora-bold-face
  "Face name to use for links.")

(defvar diaspora-emph-face 'diaspora-emph-face 
  "Face name to use for links.")

(defvar diaspora-inline-code-face 'diaspora-inline-code-face
  "Face name to use for inline code.")

(defvar diaspora-blockquote-face 'diaspora-blockquote-face
  "Face name to use for blockquote text.")

(defface diaspora-inline-code-face
  '((t :inherit font-lock-constant-face))
  "Face for inline code."
  :group 'diaspora-faces)

(defface diaspora-blockquote-face
  '((t :inherit font-lock-doc-face))
  "Face for blockquote sections."
  :group 'diaspora-faces)

(defface diaspora-header-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Base face for headers."
  :group 'diaspora-faces)

(defface diaspora-header-face-1
  '((t :inherit diaspora-header-face))
  "Face for level-1 headers."
  :group 'diaspora-faces)

(defface diaspora-header-face-2
  '((t :inherit diaspora-header-face))
  "Face for level-2 headers."
  :group 'diaspora-faces)

(defface diaspora-header-face-3
  '((t :inherit diaspora-header-face))
  "Face for level-3 headers."
  :group 'diaspora-faces)

(defface diaspora-header-face-4
  '((t :inherit diaspora-header-face))
  "Face for level-4 headers."
  :group 'diaspora-faces)

(defface diaspora-link-face
  '((t :inherit font-lock-keyword-face))
  "Face for links."
  :group 'diaspora-faces)

(defface diaspora-url-face
  '((t :inherit font-lock-string-face))
  "Face for URLs."
  :group 'diaspora-faces)

(defface diaspora-emph-face
  '((t :inherit font-lock-variable-name-face :italic t))
  "Face for italic text."
  :group 'diaspora-faces)

(defface diaspora-bold-face
  '((t :inherit font-lock-variable-name-face :bold t))
  "Face for bold text."
  :group 'diaspora-faces)

(defface diaspora-link-face
  '((t :inherit font-lock-keyword-face))
  "Face for links."
  :group 'diaspora-faces)



(defvar diaspora-mode-font-lock-keywords
  (list
;   (cons diaspora-regexp-bare-link '(2 diaspora-url-face t))
   (cons diaspora-regexp-blockquote 'diaspora-blockquote-face)
   (cons diaspora-regexp-user-entry 'diaspora-header-face-1)
   (cons diaspora-regexp-header-1 'diaspora-header-face-1)
   (cons diaspora-regexp-header-2 'diaspora-header-face-2)
   (cons diaspora-regexp-header-3 'diaspora-header-face-3)
   (cons diaspora-regexp-header-4 'diaspora-header-face-4)
   (cons diaspora-regexp-hr 'diaspora-header-face-1)
   (cons diaspora-regexp-image
         '((1 diaspora-link-face t)
           (2 diaspora-url-face t)))
   (cons diaspora-regexp-bold '(2 diaspora-bold-face))
   (cons diaspora-regexp-emph '(2 diaspora-emph-face))
   (cons diaspora-regexp-code '(2 diaspora-inline-code-face))
   (cons diaspora-regexp-email 'diaspora-link-face)
   (cons diaspora-regexp-tag 'diaspora-url-face))
  "Syntax highlighting for diaspora files.")



;; webfinger
;; see: http://devblog.joindiaspora.com/2012/01/22/how-diaspora-connects-users/
;; Probably this is not the simplest way to go...

(defun diaspora-resource-descriptor-webfinger (pod)
  "Get host resource descriptor webfinger."
  (url-retrieve (concat "https://" pod "/.well-known/host-meta") 
		(lambda (arg)
		  (save-excursion
		    (goto-char (point-min))
		    (search-forward-regexp diaspora-regex-webfinger-query))
		  (setq diaspora-resource-descriptor-webfinger-string (match-string-no-properties 1))))
  diaspora-resource-descriptor-webfinger-string)

(defun diaspora-webfinger (pod user)
  "Returns a list with webfinger with the form PROFILE-PAGE GUID HCARD ATOM D*PUBLICKEY"
  (diaspora-resource-descriptor-webfinger pod)
  (url-retrieve (concat diaspora-resource-descriptor-webfinger-string user "@" pod)
		(lambda (arg) 
		  (setq diaspora-webfinger-list 
			(mapcar (lambda (x)
				  (save-excursion
				    (goto-char (point-min))
				    (search-forward-regexp x)
				    (match-string-no-properties 1))) 
				diaspora-regexp-webfinger-all)))))

(defcustom diaspora-regexp-webfinger-query
  "<Link rel=\'lrdd\'\n[\s-]*template=\'\\(.*\\)\{uri\}\'>"
  "Regular expression for resource-descriptor-webfinger."
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-webfinger-hcard
  "<Link rel=\"http://microformats.org/profile/hcard\" type=\"text/html\" href=\"\\(.*\\)\"/>"
  "regex-webfinger-hcard"
  :type 'regexp
  :group 'diaspora)


(defcustom diaspora-regexp-webfinger-guid
"<Link rel=\"http://joindiaspora.com/guid\" type = \'text/html\' href=\"\\(.*\\)\"/>"
  "regexp-webfinger-guid"
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-webfinger-profile-page
"<Link rel=\'http://webfinger.net/rel/profile-page\' type=\'text/html\' href=\"\\(.*\\)\"/>"
  ""
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-webfinger-atom
    "<Link rel=\"http://schemas.google.com/g/2010#updates-from\" type=\"application/atom\\+xml\" href=\"\\(.*\\)\"/>" 
    "regex-webfinger-atom"
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-webfinger-publickey
  "<Link rel=\"diaspora-public-key\" type = \'RSA\' href=\"\\(.*\\)\"/>"
  "webfinger-publickey"
  :type 'regexp
  :group 'diaspora)

(defvar diaspora-regexp-webfinger-all
  (list diaspora-regexp-webfinger-profile-page
	diaspora-regexp-webfinger-guid
	diaspora-regexp-webfinger-hcard
	diaspora-regexp-webfinger-atom
	diaspora-regexp-webfinger-publickey)
  "List of all the regexp used to webfinger.")

(defun diaspora-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "diaspora.el, version %s" diaspora-el-version))

;;;###autoload
(define-derived-mode diaspora-mode text-mode "diaspora"
  "Major mode for output from \\[diaspora*]."
  (set (make-local-variable 'font-lock-defaults)
       '(diaspora-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t)
  (use-local-map diaspora-mode-map)
  (run-hooks 'diaspora-mode-hook))



(defun diaspora-url (location)
  "Make the URL according to the `diaspora-pod'(pod selected)."
  (format "%s://%s/%s" 
	  (if diaspora-secure-pod
	      "https"
	    "http")
	  diaspora-pod 
	  location))

(defun diaspora-url-json (location)
  "Make the URL as in `diaspora-url' but for retrieving JSON formats pages, according to the `diaspora-pod' (pod selected)."
  (diaspora-url
   (format "%s.json" location)))


(defun diaspora-post-comment-url (post-id)
  "Return the URL for posting a comment for the post with id post-id"
  (diaspora-url 
   (format "%s/%s/%s"
	   diaspora-single-message-url
	   (if (numberp post-id)
	       (number-to-string post-id)
	     post-id)
	   diaspora-comment-name)))

(defun diaspora-get-comment-url (post-id)
  (diaspora-url
   (format "%s/%s/%s.json" diaspora-single-message-url 
	   (if (numberp post-id)
	       (number-to-string post-id)
	     post-id)
	   diaspora-comment-name)))

(provide 'diaspora)

;;; diaspora.el ends here.

