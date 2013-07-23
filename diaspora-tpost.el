;;; diaspora-tpost.el --- 
;; 
;; Filename: diaspora-tpost.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: mar jun 11 10:20:48 2013 (-0300)
;; Version: 
;; Last-Updated: mar jun 11 11:55:06 2013 (-0300)
;;           By: Christian
;;     Update #: 7
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
;; Here goes the post structure data type.
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

(defconst tpost-field-elements '(id guid name diaspora-id text date avatar photos amount-comments amount-likes amount-reshares likes public provider-name post-type)
  "This are the fields accepted by the tpost structure.")

;; ====================================================================================================
					; TPOST API FUNCTIONS
;; This are external functions.

(defun tpost-new ()
  "Create a new structure with basic or null information and return it."
  (make-hash-table)
  )

(defun tpost-get-value (tpost field)
  "Return a given FIELD value.

FIELD must be one of the elements listed in `tpost-field-elements'."
  (if (tpost-is-field-correct field)
      (gethash field tpost)
    )
  )

(defun tpost-set-value (tpost field value )
  "Set a new VALUE for a FIELD.

FILED must be one of the elements listed in `tpost-field-elements'."
  (if (tpost-is-field-correct field)
      (puthash field value tpost)
    )
  )

;; ====================================================================================================
;; It is not recommended to use functions from here up to the end.

(defun tpost-is-field-correct (field)
  "Find a value given its FIELD.

Return t if founded in `tpost-field-elements', nil otherwise."
  (let ((item (car tpost-field-elements))
	(next (cdr tpost-field-elements))
	(founded nil))
    (while (and (not founded)
		next)
      (setq founded (eq item field))
      (setq item (car next))
      (setq next (cdr next))
      )
    founded
    )
  )

(provide 'diaspora-tpost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diaspora-tpost.el ends here
