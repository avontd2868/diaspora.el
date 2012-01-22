;;; diaspora.el --- Simple Emacs-based client for diaspora*

;; Author: Tiago Charters de Azevedo <tca@diale.org>
;; Maintainer: Tiago Charters de Azevedo <tca@diale.org>
;; Created: Jan 16, 2012
;; Version: .0
;; Keywords: diaspora*
;; URL: http://diale.org/diaspora.html

;; Copyright (c) 2011 Tiago Charters de Azevedo, Christian Giménez
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

;, Streaming 

(defun diaspora-show-stream (status &optional new-buffer-name)
  "Show what was recieved in a new buffer.
If new-buffer-name is given then, the new buffer will have that name, 
if not, the buffer called \"Diáspora Stream\" will be re-used or created if needed."
  ;; new-buffer-name has been given? if not, use `diaspora-stream-buffer´ as name.
  (unless new-buffer-name
    (setq new-buffer-name diaspora-stream-buffer))
  (let ((buffer (get-buffer-create new-buffer-name))
	(text (buffer-string))
	(buf-kill (current-buffer)))    
    ;; copy text and switch
    (switch-to-buffer buffer)
    (insert text)    
    ;; kill the http buffer
    (kill-buffer buf-kill)))

(defun diaspora-get-url-entry-stream (url)
  "Get the Diáspora URL and leave it in a new buffer."
  (let ((url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")
	   ("Accept-Language" . "en")
	   ("Accept-Charset" . "UTF-8"))))
    (url-retrieve-synchronously url)))


(defun diaspora-get-entry-stream ()
  "Show the entry stream. 
First look for the JSON file at `diaspora-entry-stream-url' and then parse it.
I expect to be already logged in. Use `diaspora' for log-in."
  (interactive)  
  (diaspora-ask) 
  (diaspora-authenticity-token diaspora-sign-in-url) ;; Get the authenticity token
  (let ((buff (diaspora-get-url-entry-stream diaspora-entry-stream-url)))
    (with-current-buffer buff
      ;; Delete the HTTP header...
      (goto-char (point-min))
      (search-forward "\n\n")      
      (delete-region (point-min) (match-beginning 0))
      ;; Parse JSON...
      (diaspora-parse-json))
    ;; Delete HTTP Buffer
    (kill-buffer buff)))

(defun diaspora-show-message (parsed-message &optional buffer)
  "Show a parsed message in a given buffer."
  (with-current-buffer buffer
    (let ( (name (cdr (assoc 'name (assoc 'author parsed-message))))
	  (diaspora_id (cdr (assoc 'diaspora_id (assoc 'author parsed-message))))
	  (text (cdr (assoc 'text parsed-message)))
	  (date (cdr (assoc 'created_at parsed-message)))
	  (amount-comments (cdr (assoc 'comments_count parsed-message)))
	  (amount-likes (cdr (assoc 'likes_count parsed-message))))
      (insert (format "---\n%s(%s):\n%s\n\n" name diaspora_id text))))
  (diaspora-mode))

(defun diaspora-get-entry-stream-tag (tag)
  "Get stream of tag. Just an idea... needs working."
  (let ((buff (diaspora-get-url-entry-stream
	       (concat "https://joindiaspora.com/tags/" tag ".json"))))
    (with-current-buffer buff
      (goto-char (point-min))
      (search-forward "\n\n")      
      (delete-region (point-min) (match-beginning 0))
      (diaspora-parse-json))
    (kill-buffer buff)))

(defun diaspora-parse-json (&optional status)
  "Parse de JSON entry stream."
  (goto-char (point-min))
  (let ((lstparsed (cdr (assoc 'posts (json-read))))
	(buff (get-buffer-create diaspora-stream-buffer)))
    (switch-to-buffer buff)
    (let ((le (length lstparsed)))
    ;; Show all elements
      (dotimes (i le)
	(diaspora-show-message (aref lstparsed i) buff)))))


(provide 'diaspora-stream)