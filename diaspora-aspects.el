;;; diaspora-aspects.el --- 
;; 
;; Filename: diaspora-aspects.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: mié feb 15 12:21:12 2012 (-0300)
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
;; Aspect support for diaspora.el
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

(require 'diaspora-stream)

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

(defconst diaspora-aspect-list-buffer-name
  "Buffer name for the list of aspects.")

(defvar diaspora-aspect-alist nil
  "This is an alist of a pair of aspects:
 ((name of the aspect . id of the aspect) ... )

This variable will get its values using the function `diaspora-get-aspects'.")

(defvar diaspora-aspects-for-post nil
  "This is a list of aspects ids. This list is used for posting, and as soon as the newly posted has been sended
to the pod, the information is discarded for a new post!
This variable is intended to be as parameter for `diaspora-post'. 
You may would like to use `diaspora-add-aspect'.")

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
		      (idna-to-unicode (buffer-substring-no-properties (point) (point-at-eol)))))))
	(push (cons name value) lista)))
    lista))

(defun diaspora-show-all-aspects ()
  "Show all aspects in a new buffer."
  (interactive)
  (diaspora-ask)
  (diaspora-get-aspects)
  ;; Create (or get) and clear a new buffer
  (with-current-buffer (get-buffer-create diaspora-aspect-list-buffer-name)
    (delete-region (point-min) (point-max))
    ;; for each element print the name...
    (dolist (elt diaspora-aspect-alist)    
      (insert (car elt) " - id: " (cdr elt) "\n")))
  (switch-to-buffer diaspora-aspect-list-buffer-name))

(defun diaspora-get-aspects (&optional reload)
  "If `diaspora-aspect-alist hasn't been generated, get an alist of aspects as key and id as values from the Diaspora pod and return the alist.
After generate the alist, save it in `diaspora-aspect-alist'.
If the reload parameter is t then, no matter what `diaspora-aspect-alist' has, reload from the `diaspora-bookmarklet-location' URL."
  (if (or reload
	  (null diaspora-aspect-alist))
      (progn 
	(diaspora-ask)
	;; We haven't loaded the aspects yet. Load it!
	(diaspora-get-authenticity-token-if-necessary)
	(with-current-buffer (diaspora-get-url-entry-stream (diaspora-url diaspora-bookmarklet-location))
	  (let ((buffer-file-coding-system 'utf-8))
	    (setq diaspora-aspect-alist (diaspora-look-for-aspects)))))    
    diaspora-aspect-alist));; We have already loaded it! return what is loaded


;; Streamming functions

(defun diaspora-get-aspects-stream ()
  "Show the aspects stream."
  (interactive)
  (diaspora-get-stream-by-name diaspora-aspects-stream-name))
 
(defun diaspora-get-stream-by-aspect (aspect-name)
  "Get all the message from an aspect stream called as ASPECT-NAME says."
  (interactive 
   (let ((string (completing-read "Aspect name?" (diaspora-get-aspects)))
	 )
     (list string))
   )
  (let ((aspect-id (assoc aspect-name diaspora-aspect-alist))
	)
    (if aspect-id
	(diaspora-get-stream (diaspora-url-json diaspora-aspects-stream-name) ;;url
			     nil ;; MAX-TIME...
			     (list (cons "a_ids[]" (cdr aspect-id))) ;;GET parameters
			     )
      (message "Aspect not founded: maybe you tiped a wrong name?")
      )
    )
  )

(provide 'diaspora-aspects)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diaspora-aspects.el ends here
