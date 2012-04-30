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

;; ********************
;; THANKS
;; ------
;;
;; Tiago Charters de Azevedo (tca@joindiaspora.com - http://diale.org )for taking part of this! 
;; Phil Hudson (philhudson@joindiaspora.com) for helping me testing and hunting some bugs!
;; ********************

; ah, ah...

(setq max-lisp-eval-depth 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'url)
(require 'url-http)
(require 'json)
(require 'font-lock)
(require 'markdown-translator)

(require 'diaspora-mode)
(require 'diaspora-urls)
(require 'diaspora-new)
(require 'diaspora-post)
(require 'diaspora-stream)
(require 'diaspora-notifications)
(require 'diaspora-aspects)
(require 'diaspora-contacts)
(require 'diaspora-messages)
(require 'diaspora-main)
(require 'diaspora-misc)
(require 'diaspora-post-edit-mode)

(defconst diaspora-el-version ".0"
  "This version of diaspora*-el.")

(defconst diaspora-tag "diaspora-el"
  "This is the tag (without \"#\") where you can see the latest news of diaspora-el!"
  )

(defgroup diaspora nil 
  "A mode for diaspora* stream view and posting."
  :group 'applications)

;;; User variable:

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

(defcustom diaspora-image-history-file
  "~/.diaspora/image-sended-history"
  "This file will store all the images id an information that you may need when a post fails.

If you send an image to diáspora, the server will save it until you send a post with its id. 
If posting goes wrong for some reason, and the variable `diaspora-images-posted' has been cleared, you miss all the ids of the images resulting in sending them again.

Well, thanks to this file, you don't need to send the images again, just look at the id, add it to the variable `diaspora-images-posted' and that's all."
  :type 'file
  :group 'diaspora
  )

(defcustom diaspora-show-images-by-default
  nil
  "Loads images by default at start."
  :type 'boolean
  :group 'diaspora)


(defcustom diaspora-show-user-avatar t
   "Show user images beside each users entry."
   :type 'boolean
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

;;; Internal Variables:

(defvar  diaspora-webfinger-list nil
  "")

(defvar diaspora-auth-token nil
  "Authenticity token variable name.")

(defvar  diaspora-resource-descriptor-webfinger-string nil
  "")


(defvar diaspora-post-buffer "*diaspora post*"
  "The name of the diaspora post buffer.")

;;; User Functions:

(defun diaspora ()
  "Make all dirs if they don' exist and set `diaspora-username' 
and  `diaspora-password' if they weren't."
  (interactive)
  (diaspora-make-dirs)
  (diaspora-ask)
  (diaspora-main)
  )

(defun diaspora-customize ()
  "Run Customization utility for customize D*.el."
  (interactive)
  (customize-group "diaspora")
  )

(defun diaspora-login ()
  "Ask for username and password interactivelly. 

Use `diaspora-ask'."
  (interactive)
  (diaspora-ask t)
  )
  
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

(defun diaspora-clean-cache ()
  (interactive)
  (shell-command (concat "rm -f " diaspora-image-directory "*")))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun diaspora-hide-markdown ()
  "Hide markdown codes leaving only markdown text with colors and face properties."
  (interactive)
  (let ((inhibit-read-only t))
    (markdown-trans-apply)
    (markdown-trans-hide)
    )    
  )

(defun diaspora-show-markdown ()
  "Show markdown codes."
  (interactive)
  (let ((inhibit-read-only t))
    (markdown-trans-show)
    )
  )


(defun diaspora-remove-bad-chars ()
  "Remove characters that looks ugly."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (replace-string "" "")
      )
    )
  )

(defun diaspora-replace-bad-links ()
  "Replace links like \"[](http://something.com\" or \"![](http://something.com/image.png\". 

This links when parsing doesn't show anything because there is no label. 
Change it to something like: \"[nolabel](http://...)\"."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp diaspora-regexp-image nil t)
      (if (string= (match-string-no-properties 1) "[]")
	  (replace-match "[nolabel]" nil nil nil 1))
      )
    )
  )

(defun diaspora-get-username (username-at-pod)
  "Get only the username from the string \"username@pod\"."
  (car (split-string username-at-pod "@" t))
  )

(defun diaspora-get-pod (username-at-pod)
  "Get only the username from the string \"username@pod\"."
  (cadr (split-string username-at-pod "@" t))
  )

(defcustom diaspora-pod-list
  '("joindiaspora.com" "diasp.org" "calispora.org" "despora.de" "diasp.be"
    "diasp.de" "diasp.eu" "diasp0ra.ca" "diaspora.cloudid.net" "diaspora.compadre.dk"
    "diaspora.digitalinsanity.de" "diaspora.eigenlab.org" "diaspora.podzimek.org"
    "diaspora.re" "diaspora.sceal.ie" "diasporaserbia.org" "diaspora.subsignal.org"
    "diaspora.urbanabydos.ca" "dipod.org" "dipod.es" "foobar.cx" "free-beer.ch"
    "hasst-euch-alle.de" "li-la.de:3000" "londondiaspora.org" "my-seed.com" 
    "nerdpol.ch" "www.nomed.de" "ottospora.nl" "pod.geraspora.de" "poddery.com"
    "privit.us" "soc.ragriz.net" "spargo.me" "spora.com.ua" "the.diasperse.com"
    "wk3.org" "yaspora.com" "diaspora.danapriesing.net" "diasp.3towers.de"
    "d-aspora.ru" "diaspora.erchache2000.es"
    )
  "List of Pods used by `diaspora-set-pod' function."
  :group 'diaspora
  :type '(repeat string)
  )

(defun diaspora-set-pod (other-pod)
  "Change the pod for the next connection. This doesn't set it permanently, just temporary.

This command is usefull when you want to change the POD for now."
  (interactive 
   (let ((string (completing-read "POD URL?" diaspora-pod-list nil nil))
	 )
     (list string))
   )
  (setq diaspora-pod other-pod)
  (message (format "%s %s" "Setted temporary to " other-pod))
  )


(provide 'diaspora)

;;; diaspora.el ends here.

