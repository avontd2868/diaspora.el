;;; diaspora-http.el --- 
;; 
;; Filename: diaspora-http.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: mié jun  5 00:04:08 2013 (-0300)
;; Version: 
;; Last-Updated: mar jul 23 03:08:31 2013 (-0300)
;;           By: Christian
;;     Update #: 86
;; URL: 
;; Doc URL: 
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
;; Library for managing HTTP conections to D*.
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

(require 'url)
(require 'json)

(require 'diaspora-urls)
(require 'diaspora-http-errors)
(require 'diaspora-tpost)


(defun diaspora-http-connect (user pass fnc)
  "Connect to an account in D* using HTTP.

*This function is part of the API at the Connection Level.*


Get the session cookie so we don't need later authentication.

The procedure is simple: just send the POST data for the login form.

FNC is a function with one parameter: the auth-token."
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		    (list (cons "user[username]" user)
			  (cons "user[password]" pass)
			  (cons "user[remember_me]" "1")
			  (cons "utf8" "✓"))
		    "&")))

    (url-retrieve (diaspora-url-sign-in) 'diaspora-cb-connect (list fnc))

    )  
  )



(defun diaspora-http-get-message (id fnc)
  "Retrieve the message and return it as a tpost structure.

ID a string with the id of the message. The id can be seen at the URL of the message.
FNC a callback function with one parameter: a retrieved tpost structure."
  (let ((url-request-method "GET")
	)
	

    (url-retrieve (diaspora-url-post id) 'diaspora-cb-get-message (list fnc))
    )
  )

;; ====================================================================================================
					; Private functions

(defun diaspora-cb-get-message (status fnc)
  "Callback function for `diaspora-http-get-message'.

After finishing retrieving the message, execute FNC function with one parameter: the retrieved post as tpost structure."

  (diaspora-http-remove-header)
  
  (funcall fnc (diaspora-http-parse-message))
  )

(defun diaspora-http-parse-message ()
  "Parse the JSON message and return a tpost structure with all the information gathered."
  (let ((parsed (json-read))
	(post (make-diaspora-tpost))
	)

    (setf (diaspora-tpost-text post) (cdr (assoc 'text (cdr parsed))))
    (setf (diaspora-tpost-id) (cdr (assoc 'id (cdr parsed))))
    (setf (diaspora-tpost-date) (cdr (assoc 'created_at (cdr parsed))))
    (setf (diaspora-tpost-guid) (cdr (assoc 'guid (cdr parsed))))
    
    ;; Author data
    (setf (diaspora-tpost-author-name) (cdr (assoc 'name (cdr (assoc 'author (cdr parsed))))))
    (setf (diaspora-tpost-author-guid) (cdr (assoc 'guid (cdr (assoc 'author (cdr parsed))))))
    (setf (diaspora-tpost-author-id) (cdr (assoc 'id (cdr (assoc 'author (cdr parsed))))))
    (setf (diaspora-tpost-author-diaspora-id) (cdr (assoc 'diaspora_id (cdr (assoc 'author (cdr parsed))))))
    (setf (diaspora-tpost-author-avatar) (cdr (assoc 'small (cdr (assoc 'avatar (cdr (assoc 'author (cdr parsed))))))))
    )
  )

(defun diaspora-cb-connect (status fnc)
  "Callback function for `diaspora-http-connect'.
After finishing checking errors, it execute FNC function with no parameter."
  (let ((errornum (diaspora-http-error-get-number)))
    (unless (equal errornum 200)
      (cond
       ((equal errornum 401)
	(error "Username, Password or Pod is Incorrect!" "Take a look at your username and password. If it is correct, take a look `diaspora-pod'. If not, the signing URL may not be the correct one, check `diaspora-sign-in-url'."))
       ((equal errornum 404)
	(error "The sign in page is wrong!" "Check `diaspora-sign-in-url' and other diaspora-*-url variables."))	       
       ((equal errornum 302)
	;; Login was successful but we're being redirected! We need to load the new page a take auth-token from there
	(error "We're being Redirected! I don't know what else to do yet... means: This is a TODO!")	
	)
       )      
      )    
    
    (funcall fnc)
    )
  )


(defun diaspora-http-get-remember-cookie ()
  "Search in the current buffer for the HTTP header \"Set-Cookie\" which has the \"remember_user_token\" cookie. 

Return the string or nil if not founded."
  (diaspora-http-get-cookie "remember_user_token")
  )

(defun diaspora-http-get-session-cookie ()
  "Search in the current buffer for the HTTP header \"Set-Cookie\" which has the \"_diaspora_session\" cookie. 

Return the string or nil if not founded."
  (diaspora-http-get-cookie "_diaspora_session")
  )


(defun diaspora-http-get-cookie (name)  
  "Search the \"Set-Cookie\" HTTP header field and return the cookie that matchs with its name.

Returns the string or nil if not founded."

  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp (concat "Set-Cookie:.*" name "=\\([^;]+\\)")
			       nil 
			       t)
	(match-string 1)
      nil
      )
    )  
  )

(defun diaspora-http-remove-header ()
  "Remove the HTTP header from the current buffer."
  (goto-char (point-min))
  (search-forward "\n\n" nil t)
  (delete-region (point-min) (match-end 0))
  )


(provide 'diaspora-http)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diaspora-http.el ends here
