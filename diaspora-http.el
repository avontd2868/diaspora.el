;;; diaspora-http.el --- 
;; 
;; Filename: diaspora-http.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: mié jun  5 00:04:08 2013 (-0300)
;; Version: 
;; Last-Updated: mié jun  5 15:36:27 2013 (-0300)
;;           By: Christian
;;     Update #: 46
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

(require 'diaspora-urls)
(require 'diaspora-http-errors)
(require 'diaspora-misc)
(require 'url)


(defun diaspora-connect (user pass)
  "Connect to an account in D* using HTTP.

*This function is part of the API at the Connection Level.*
"
  
  (diaspora-get-session-cookie user pass)  
  )

(defun diaspora-get-entry-stream ()
  "Get the entry stream and return a list of posts.

*This function is part of the API at the Connection Level.*
"
  
  (diaspora-get-stream (diaspora-url-json diaspora-entry-stream-name))
  
  )






;; ----------------------------------------------------------------------------------------------------

(defun diaspora-get-session-cookie (user pass)
  "Get the session cookie so we don't need later authentication.

The procedure is simple: just send the POST data for the login form."
  (let ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		    (list (cons "user[username]" user)
			  (cons "user[password]" pass)
			  (cons "user[remember_me]" "0")
			  (cons "utf8" "✓"))
		    "&")))


    ;; (diaspora-debug-msg "***POSTing:")
    ;; (diaspora-debug-msg (diaspora-url-sign-in))
    ;; (diaspora-debug-msg url-request-data)

    (url-retrieve (diaspora-url-sign-in) 'diaspora-cb-get-session-cookie))
  )

(defun diaspora-cb-get-session-cookie (status)
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
    )
  )


(defun diaspora-get-stream (url &optional max-time lst-get-parameters lst-post-parameters)
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

	  ;; (diaspora-debug-msg "***GETing:")
	  ;; (diaspora-debug-msg url)
	  ;; (diaspora-debug-msg url-request-data)
	  
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

	;; (diaspora-debug-msg "***GETing:")
	;; (diaspora-debug-msg url)
	;; (diaspora-debug-msg url-request-data)

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



(provide 'diaspora-http)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diaspora-http.el ends here
