;;; diaspora-messages.el --- 
;; 
;; Filename: diaspora-messages.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: lun abr 16 23:38:38 2012 (-0300)
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

(require 'diaspora-urls)
(require 'htmlr)

(defconst diaspora-messages-buffer-name "*Diaspora Messages*"
  "This is the buffer-name of the diaspora messages list.")
(defconst diaspora-message-buffer-name "*Diaspora One Message*"
  "This is the buffer-name for showing a diaspora message."
  )

(defun diaspora-messages ()
  "List the messages that are in the convesations-url."
  (interactive)
  (let ((buffer-to (get-buffer-create diaspora-messages-buffer-name))
	(inhibit-read-only t)
	)
    (with-current-buffer buffer-to
      (delete-region (point-min) (point-max))
      (diaspora-mode)
      (with-current-buffer (diaspora-messages-get-list)
	(diaspora-messages-parse-json-msg-list (current-buffer) buffer-to)
	) 
      (goto-char (point-min))
      )
    (switch-to-buffer buffer-to)
    )
  )
  

(defun diaspora-messages-get-list ()
  "Get all the list of messages from the URL made by `diaspora-url-json' and `diaspora-messages-url'. 
Then return a temporary buffer with the messages in JSON format."
  (diaspora-ask)
  (diaspora-get-authenticity-token-if-necessary)
  (let ((buff (diaspora-get-url (diaspora-url-json diaspora-messages-url)))
	)
    (with-current-buffer buff
      (diaspora-delete-http-header)
      )
    buff
    )
  )

(defun diaspora-messages-parse-json-msg-list (buffer-from buffer-to)
  "Read the JSON message list from buffer-from and parse it, write the results in buffer-to according to the function
`diaspora-messages-show-list-element'."
  (with-current-buffer buffer-from
    (goto-char (point-min))
    (let* ((json-lst (json-read))
	   (le (length json-lst))
	   )
      (with-current-buffer buffer-to
	(dotimes (i le)
	  (diaspora-messages-show-list-element (car (aref json-lst i)))
	  )
	)
      )
    )
  )

(defun diaspora-messages-show-list-element (json-elt)
  "Show a list element reading the information from a JSON element part."
  (let ((id (cdr (assoc 'id json-elt)))
	(subject (cdr (assoc 'subject json-elt)))
	(updated (cdr (assoc 'updated_at json-elt)))
	(created (cdr (assoc 'created_at json-elt)))
	(author-id (cdr (assoc 'author_id json-elt)))
	)
    (insert (concat
	     (propertize 
	      "          ====================          \n"
	      'diaspora-message-separator t)))
    (insert subject "\n")
    (insert (format "%s\nAuthor-id %s" updated author-id) "\n")
    (insert (diaspora-add-link-to-userstream "Autor Stream" author-id) " | ")
    (insert (diaspora-add-link-to-message "Read" id) "\n")    
    )
  )

(defvar diaspora-message-show-msg-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'diaspora-message-at-point-show)
    (define-key map [mouse-2] 'diaspora-message-at-point-show)
    map)
  "Keymap used when the user clics on a name link.")

(defun diaspora-add-link-to-message (text msg-id)
  (propertize 
   text
   'mouse-face 'highlight
   'face "link"
   'keymap diaspora-message-show-msg-map
   'diaspora-conversation-id msg-id ;;We use diaspora-conversation-id because of misinterpretation
   'diaspora-is-link-to-pub t
   'help-echo "Click here to see this message in new buffer.")
  )

(defun diaspora-message-at-point-show (&rest r)
  "Look the message id near point and show it using `diaspora-message-show'."
  (interactive)
  (diaspora-message-show (diaspora-get-conversation-id-near-point))
  )

(defun diaspora-get-conversation-id-near-point ()
  "Get the diaspora-conversation-id property value searching from point.
Use it for getting the nearest id post number when selecting a message."
  (get-text-property (+ 1 (previous-single-property-change (+ (point) 1) 'diaspora-conversation-id))
		     'diaspora-conversation-id)
  )



(defun diaspora-message-show (msg-id)
  "Show a conversation message in a new buffer."
  (diaspora-ask)
  (diaspora-get-authenticity-token-if-necessary)
  (let ((buffer-to (get-buffer-create diaspora-message-buffer-name))
	(text nil)
	)
    (with-current-buffer (diaspora-get-url (diaspora-messages-url msg-id))
      (let ((buffer-file-coding-system 'utf-8))
	(diaspora-delete-http-header)
	(diaspora-message-delete-unnecessary)
	(diaspora-message-replace-necessary)
	(setq text (buffer-string))
	)
      )
    (with-current-buffer buffer-to
      (let ((inhibit-read-only t)
	    (buffer-file-coding-system 'utf-8)
	    )
	(delete-region (point-min) (point-max))
	(insert (string-as-multibyte text))
	(goto-char (point-min))
	(htmlr-render)
	(goto-char (point-min))
	(switch-to-buffer buffer-to)
	)
      )    
    )  
  )
  
(defun diaspora-message-delete-unnecessary ()
  "Remove whatever is unneded for displaying a message."
  (when (search-forward "<h3 class='ltr'>" nil t)
    (delete-region (point-min) (match-beginning 0))
    )  
  (when (search-forward "<textarea cols=" nil t) 
    (delete-region (match-beginning 0) (point-max))
    )
  )

(defun diaspora-message-replace-necessary ()
  "Replace text so you can see the message in a better way."
  (save-excursion 
    (goto-char (point-min))
    (while (search-forward "<div class='ltr'>" nil t)      
      (goto-char (match-end 0))
      (insert "\n<hr />\n")
      )
    )
  )


(provide 'diaspora-messages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diaspora-messages.el ends here
