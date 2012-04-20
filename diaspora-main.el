;;; diaspora-main.el --- 
;; 
;; Filename: diaspora-main.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: vie abr 20 09:40:20 2012 (-0300)
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

(defconst diaspora-main-buffer-name "D*"
  "This is the name of the main buffer."
  )

(defcustom  diaspora-main-list-of-options 
  '(
    ("Notifications" . diaspora-get-notifications)
    ("Main Stream" . diaspora-get-entry-stream)
    "Aspects"
    ("Get Aspect Stream" . diaspora-get-aspects-stream)
    ("Get Aspect List" . diaspora-show-all-aspects)    
    "Other Streams" ;; This is a particular item
    ("Get Tag Stream" . diaspora-get-stream-by-tag)
    ("Get Contact Stream" . diaspora-get-stream-by-contact)
    ("Get Stream using a Username" . diaspora-get-stream-by-username)
    "Personal Conversations(Messages)"
    ("Messages" . diaspora-messages)
    "Posting"
    ("Post a text" . diaspora-post-to)
    )
  "This is a list of elements to show in the main menu.

Is a list of cons with the name of the option and the function to call."
  :group 'diaspora
  :type '(alist :key-type (group string) :value-type (group function))
  )
    

(defun diaspora-main ()
  "¡Show the main menu!"
  (interactive)

  (with-current-buffer (get-buffer-create diaspora-main-buffer-name)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (diaspora-main-insert-title)
      (diaspora-main-insert-list-of-options)
      (goto-char (point-min))
      (diaspora-main-mode)
      )
    )  
  (switch-to-buffer diaspora-main-buffer-name)
  )

(defface diaspora-main-title-face 
  '((t :weight bold :height 2.0 :foreground "spring green" ))
  "This is the MAIN TITLE face."
  )

(defun diaspora-main-insert-title ()
  "Insert a title in the current buffer."  
  (insert "Welcome to D*.el!") 
  (center-line)
  (insert "\n\n")
  )

(defun diaspora-main-insert-list-of-options ()
  "Write down all the list of options."
  (dolist (option diaspora-main-list-of-options)
    (if (listp option)
	(insert (propertize (car option)
			    'diaspora-main-option (car option)
			    'mouse-face 'highlight
			    'help-echo "Click or press Enter to execute the option."
			    )
		"\n")
      (insert "\n*" option "*\n")
      )
    )
  )

(defun diaspora-main-execute-option (&rest r)
  "Execute the selected option."
  (interactive)
  (let ((option (get-text-property (point) 'diaspora-main-option)))
    (if option
	(call-interactively (cdr (assoc option diaspora-main-list-of-options)))
      )
    )
  )

(defface diaspora-main-mode-option-face
  '((t :inherit 'diaspora-buttons-elements-face))
  "Face for the options in the Main buffer."
  :group 'diaspora-faces
  )

(defface diaspora-main-mode-subtitle-face
  '((t :inherit 'diaspora-header-face-2 :weight bold))  
  "Face for each of the sub-titles in Main buffer."
  :group 'diaspora-faces
  )

(defvar diaspora-main-mode-map
  (let ((diaspora-main-mode-map (make-sparse-keymap)))
    (define-key diaspora-main-mode-map [return] 'diaspora-main-execute-option)
    (define-key diaspora-main-mode-map [mouse-2] 'diaspora-main-execute-option)
    diaspora-main-mode-map
    )
  )

(defvar diaspora-main-mode-font-lock-keywords
  '((
     ("[[:space:]]*Welcome.*$" . 'diaspora-main-title-face)
     ("^\*.*\*$" . 'diaspora-main-mode-subtitle-face)
     ("^[^\*[:space:]].+?$" . 'diaspora-main-mode-option-face)
     ))
  "Syntax highlighting for D*")

(define-derived-mode diaspora-main-mode nil "D*-mode"
  "Major mode for D* main buffer."
  (set (make-local-variable 'font-lock-defaults)
			    diaspora-main-mode-font-lock-keywords)
  (use-local-map diaspora-main-mode-map)
  (set (make-local-variable 'buffer-read-only) t)
  )

(provide 'diaspora-main)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diaspora-main.el ends here
