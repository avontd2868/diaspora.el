;;; diaspora-http-errors.el --- 
;; 
;; Filename: diaspora-http-errors.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: jue feb 28 22:53:08 2013 (-0300)
;; Version: 
;; Last-Updated: jue feb 28 23:04:39 2013 (-0300)
;;           By: Christian
;;     Update #: 16
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
;; 
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

(defconst diaspora-http-error-regexp "^HTTP/1\\.1 \\([[:digit:]]+\\) \\(.*\\)$"
  "Error regexp of HTTP 1.1")

(defun diaspora-http-error-get-number ()
  "Return the HTTP 1.1 error number. 200 if there is no error."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp diaspora-http-error-regexp nil t)
	(progn
	  (string-to-number (match-string 1))
	  )
      nil
      )
    )
  )

(defun diaspora-http-error-get-string ()
  "Return the HTTP 1.1 error string. OK if there is no error."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp diaspora-http-error-regexp nil t)
	(progn
	  (match-string 2)
	  )
      nil
      )
    )
  )

(provide 'diaspora-http-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diaspora-http-errors.el ends here
