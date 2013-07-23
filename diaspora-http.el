;;; diaspora-http.el --- 
;; 
;; Filename: diaspora-http.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: mié jun  5 00:04:08 2013 (-0300)
;; Version: 
;; Last-Updated: mar jul 23 02:04:33 2013 (-0300)
;;           By: Christian
;;     Update #: 59
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

(require 'diaspora-urls)
(require 'diaspora-http-errors)


(defun diaspora-connect (user pass fnc)
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

    (url-retrieve (diaspora-url-sign-in) 'diaspora-cb-get-session-cookie (list fnc))

    )  
  )


;; ====================================================================================================
					; Private functions


(defun diaspora-cb-get-session-cookie (status fnc)
  "Callback function for `diaspora-get-session-cookie'.
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


(provide 'diaspora-http)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diaspora-http.el ends here
