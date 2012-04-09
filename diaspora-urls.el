;;; diaspora-urls.el --- 
;; 
;; Filename: diaspora-urls.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: mié abr  4 11:52:27 2012 (-0300)
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
;; This library provides functions to create URLs related to Diaspora.
;;
;; Please, don't create URLs using `concat' or other function! Create your function here if necessary and call it from here.
;; This is necessary because URLs must have the `diaspora-pod' and other suitable elements.
;;
;; Make sure that always calls this functions for an URL and don't create any URL in others libraries... 
;; it leads to inconcistences when creating URLs.
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

;; It is a user facility that `diaspora-secure-pod' and `diaspora-pod' is added in the customization group "diaspora".

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

(defgroup diaspora-urls nil
  "Diaspora-urls URL Generator for Diaspora."
  :group 'diaspora
  :tag "Diaspora's URLs Generation."
  :version "23.0")

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


(defcustom diaspora-likes-name
  "likes"
  "Name for sending likes POSTs."
  :type 'string
  :group 'diaspora-streams)

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

(defun diaspora-image-path (image-name)
  "Return the temporal image path."
  (concat diaspora-image-directory 
	  (diaspora-image-filter-out-bad-chars image-name))
  )

(defun diaspora-image-filter-out-bad-chars (image-name)
  (let ((name image-name))	
    (while (string-match "[&\\?><|%]" name)
      (setq name (replace-match "" nil t name))
      )
    name
    )
  )
  

(defun diaspora-image-path-from-url (image-url &optional user-id)
  "Return the temporal image path from the url where it has been dowloaded."
  (concat diaspora-image-directory
	  (if user-id 
	      (concat user-id "-"))
	  (diaspora-image-filter-out-bad-chars (file-name-nondirectory image-url))
	  )
  )


(defun diaspora-likes-url (post-id)
  (diaspora-url
   (format "%s/%s/%s"
	   diaspora-single-message-url
	   (if (numberp post-id)
	       (number-to-string post-id)
	     post-id)
	   diaspora-likes-name)
   )
  )

(provide 'diaspora-urls)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diaspora-urls.el ends here
