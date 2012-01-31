;;; diaspora.el --- Simple Emacs-based client for diaspora*

;; Author: Tiago Charters de Azevedo <tca@diale.org>
;; Maintainer: Tiago Charters de Azevedo <tca@diale.org>
;; Created: Jan 16, 2012
;; Version: .0
;; Keywords: diaspora*
;; URL: http://diale.org/diaspora.html

;; Copyright (c) 2012 Tiago Charters de Azevedo
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

;; Streaming ideas


;; file with leftovers!!!!


(defun diaspora-get-url(url)
  "Get a diaspora URL and leave it in a new buffer."
  (let ((url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")
	   ("Accept-Language" . "en")
	   ("Accept-Charset" . "UTF-8"))))
    (url-retrieve-synchronously url)))

(defun diaspora-delete-http-header ()
  "Delete the first lines that is the HTTP header in the current buffer.
This is used after getting a stream or any URL in JSON format."
   (goto-char (point-min))
   (search-forward "\n\n")      
   (delete-region (point-min) (match-beginning 0)))


;; Getting info from JSON format


(defun diaspora-extract-json (e a)
  (cond ((listp a)
	 (cdr (assoc e a)))
	((vectorp a)
	 (cdr (assoc e (aref a 0))))))

(defun diaspora-extract-json-list (e a)
  (cond (e
	 (diaspora-extract-json-list (cdr e) 
			 (diaspora-extract-json (car e) a)))
	(a)))

(defun diaspora-json-car (x)
  (cond ((listp x)
	 (car x))
	((vectorp x)
	 (car (append x nil)))))

(defun diaspora-json-cdr (x)
  (cond ((listp x)
	 (cdr x))
	((vectorp x)
	 (cdr (append x nil)))))

(defun diaspora-json-read (url)
  (interactive)
  (let ((http-buffer (diaspora-get-url url)))
    (with-current-buffer http-buffer
      (diaspora-delete-http-header)
      (let ((stream-parsed (json-read)))
	 stream-parsed))))

(defun diaspora-stream ()
  (interactive)
  (diaspora-ask)
  (diaspora-authenticity-token diaspora-sign-in-url) 
  (let ((buffer (diaspora-get-url diaspora-entry-stream-url)))
    (with-current-buffer buffer
      (diaspora-delete-http-header)
      (diaspora-parse-json)
      (diaspora-mode)
      (goto-char (point-min)))))

(defun diaspora-parse-json (name)
  "Parse de JSON entry `name' stream."
  (goto-char (point-min))
  ;; Create a new buffer called according `diaspora-buffer' say 
  ;; and parse the json code into lists.
  (let ((lstparsed (cdr (assoc name (json-read))))
	(buff (get-buffer-create diaspora-stream-buffer))) 
    ;; clean the new buffer
    (switch-to-buffer buff)
    (let ((le (length lstparsed)))
	  ;(inhibit-read-only t))
      (delete-region (point-min) (point-max))
    ;; Show all elements
      (dotimes (i le)
	(diaspora-show-message (aref lstparsed i) buff)))))

