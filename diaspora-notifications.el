;;; diaspora-notifications.el --- 
;; 
;; Filename: diaspora-notifications.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: sáb ene 28 21:25:27 2012 (-0300)
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

(defvar diaspora-notifications-url "https://joindiaspora.com/notifications.json"
  "This is the URL where I can get in JSON format the notifications.")

(defvar diaspora-notifications-buffer-name "*diaspora notifications*"
  "This is the name of the buffer that shows notifications from D*.")

(defun diaspora-get-notifications () 
  "Get notifications from diáspora and show them in a new buffer"
  (interactive)
  (let ((http-buff (diaspora-get-url-entry-stream diaspora-notifications-url))
	(buff (get-buffer-create diaspora-notifications-buffer-name)))
    (with-current-buffer http-buff
      (diaspora-delete-http-header))
    (diaspora-parse-notifications-json http-buff buff)
    (switch-to-buffer buff)
    (with-current-buffer buff
      (let ((inhibit-read-only t))
	(w3m-buffer))
      (setq buffer-read-only t))))


(defun diaspora-show-notification (notification buffer)
  "Insert into buffer the JSON formated notification in a most human readable text."  
  (with-current-buffer buffer    
    (let ((inhibit-read-only t)
	  (type (car (car notification)))
	  (date (cdr (assoc 'updated_at (cdr (car notification)))))
	  (unread (cdr (assoc 'unread (cdr (car notification)))))
	  (target (cdr (assoc 'target_type (cdr (car notification)))))
	  (recipient (cdr (assoc 'recipient_id (cdr (car notification)))))
	  (note (cdr (assoc 'note_html (cdr (car notification))))))
      (insert (format "\n<hr />\n%s:%s<br />" date note)))))

(defun diaspora-parse-notifications-json (buffer buffer-to)
  "Parse all the notifications in the JSON that are in the given buffer, and put the result in the \"buffer-to\" buffer."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((lst-parsed (json-read))) ;;This returns an array of json notifications!
      (with-current-buffer buffer-to
	(delete-region (point-min) (point-max))
	(let ((le (length lst-parsed)))
	  (dotimes (i le)
	    (diaspora-show-notification (aref lst-parsed i) buffer-to)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diaspora-notifications.el ends here
