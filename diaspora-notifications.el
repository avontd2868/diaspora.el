;;; diaspora.el --- Simple Emacs-based client for diaspora*

;; Author: Christian Giménez
;; Maintainer: Tiago Charters de Azevedo <tca@diale.org>
;; Created: Jan 31, 2012
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


(defun diaspora-add-key-to-w3m-link-keymap ()
  "Add to the `w3m-link-map' the keys necesary to use only the keyboard."
  (interactive)
  (define-key w3m-link-map "\C-c\C-o" 'diaspora-notification-goto-link)
  )

(defun diaspora-get-notifications () 
  "Get notifications from diáspora and show them in a new buffer"
  (interactive)
  (diaspora-ask)
  (let ((http-buff (diaspora-get-url-entry-stream diaspora-notifications-url))
	(buff (get-buffer-create diaspora-notifications-buffer))
	(inhibit-read-only t))    
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
    (let ((type (car (car notification)))
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

(provide 'diaspora-notifications)