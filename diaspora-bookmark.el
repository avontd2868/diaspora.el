;;; diaspora-bookmark.el --- 

;; Copyright 2012 Christian
;;
;; Author: cnngimenez@gmail.com
;; Version: $Id: diaspora-bookmark.el,v 0.0 2012/12/14 13:38:14 cng Exp $
;; Keywords: 
;; X-URL: not distributed yet

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
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'diaspora-bookmark)

;;; Code:


(defcustom diaspora-bookmark-file "~/.diaspora/bookmarks.org"
  "Diaspora bookmarks file where we save all bookmarks in an organized way.

The file will be saved in org format."
  :type 'file
  :group 'diaspora)

(defun diaspora-bookmark-post (bookmark-name)
  "Bookmark the current post so you can review it later"
  (interactive "MName?")
  (let ((id (diaspora-get-id-message-near-point)))
    (if id
	(progn
	  (diaspora-bookmark-save-entry bookmark-name id)
	  (message "Bookmar saved.")
	  )
      (message "No message-id founded.")	
      )
    )
  )

(defun diaspora-bookmark-save-entry (name id)
  "Save an entry into the `diaspora-bookmark-file'.
The ID of the post is needed, and a NAME for reference."
  (diaspora-bookmark-create-file-if-not-exists)
  (with-current-buffer (find-file-noselect diaspora-bookmark-file)  
    (goto-char (point-max))
    (insert "    - " name " :: " (number-to-string id)
	    " ([[" 
	    (diaspora-post-url id)
	    "][Link]])\n"
	    )
    (write-file diaspora-bookmark-file nil)
    )
  )

(defun diaspora-bookmark-create-file-if-not-exists ()
  "Create the bookmark file (`diaspora-bookmark-file') if it does not exists. If it does, do nothing."
  (unless (file-exists-p diaspora-bookmark-file)
    (with-temp-file diaspora-bookmark-file 
      (rename-buffer "bookmarks")
      (org-insert-export-options-template)
      (goto-char (point-max))    
      (insert "\n\n* Bookmarks\n")
      )
    )
  )

(defun diaspora-bookmarks ()
  "Show all the bookmark list. 

Bookmark list is in org format. You can edit the file as long as you preserve the format of the lists( \"- name :: id (Link)\" ).

You can export it as HTML, PDF, etc. using the C-c C-e and choosing an output format."
  (interactive)
  (diaspora-bookmark-create-file-if-not-exists)
  (find-file diaspora-bookmark-file)
  (goto-char (point-max)))


(defun diaspora-bookmark-find-id (name)
  "Return the id looking for the NAME of the bookmark.

nil if that NAME doesn't exists."
  (with-current-buffer (find-file-noselect diaspora-bookmark-file)  
    (goto-char (point-min))
    (search-forward "Bookmarks")
    ;; search only in the lists
    (if (search-forward (concat "- " name " ::") nil t)
	(let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
	  (string-match ".* :: \\([[:digit:]]*\\)"  line)
	  (let ((id-str (match-string-no-properties 1 line)))
	    (if id-str
		(string-to-number id-str)))
	  )
      nil
      )
    )
  )


(defun diaspora-bookmark-open (name)
  "Look for NAME in the bookmarks and open that saved message."
  (interactive "MName?")
  (let ((id (diaspora-bookmark-find-id name)))
    (if id
	(diaspora-get-single-message id)
      (message "Bookmark name not founded!")))
  )

(provide 'diaspora-bookmark)

;;; diaspora-bookmark.el ends here
