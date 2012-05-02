;;; diaspora-misc.el --- 
;; 
;; Filename: diaspora-misc.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: jue abr 26 22:26:38 2012 (-0300)
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

(defun diaspora-get-keys (alist)
  "Return all the keys from an alist."
  (let ((lstout nil))
    (dolist (e alist)
      (push (car e) lstout)
      )
    lstout
    )	     
  )

(defun diaspora-get-values (alist)
  "Return all the values from an alist."
    (let ((lstout nil))
    (dolist (e alist)
      (push (cdr e) lstout)
      )
    lstout
    )	     
  )

(defun diaspora-misc-next-option (property)
  "Go to next property... if there is no more, goto the first one."
  ;;(goto-char (point-at-eol))
  (if (get-text-property (point) property)
      (diaspora-misc-next-option-1 property))
  (unless (diaspora-misc-next-option-1 property)
    ;; End of buffer! 
    (goto-char (point-min))
    (diaspora-misc-next-option-1 property)
    )
  )

(defun diaspora-misc-next-option-1 (property)
  "Go to next change in the property given by PROPERTY. 
If a change in the property is not founded(i.e. the property remains constant and unchanged until end of buffer), return nil.
If a change in the property is founded return t."
  (let ((next-option (next-single-property-change (point) property)))
    (if next-option
	(progn 
	  (goto-char next-option)
	  t)
      nil
      )
    )
  )

(provide 'diaspora-misc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diaspora-misc.el ends here
