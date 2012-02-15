;;; diaspora.el --- Simple Emacs-based client for diaspora*

;; Author: Tiago Charters de Azevedo <tca@diale.org>
;; Maintainer: Tiago Charters de Azevedo <tca@diale.org>
;; Created: Jan 16, 2012
;; Version: .0
;; Keywords: diaspora*
;; URL: http://diale.org/diaspora.html

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:

;; Save all the files in a DIR and add that DIR to `load-path'; 
;; for instance `(add-to-list 'load-path "~/emacs.el/disaspora.el/")' to your .emacs
;; Files: diaspora.el, diaspora-post.el  and diaspora-stream.el 

; ah, ah...

(setq max-lisp-eval-depth 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'url)
(require 'url-http)
(require 'json)
(require 'font-lock)

(require 'diaspora-new)
(require 'diaspora-post)
(require 'diaspora-stream)
(require 'diaspora-colors)
(require 'diaspora-notifications)

(defconst diaspora-el-version ".0"
  "This version of diaspora*-el.")

(defgroup diaspora nil 
  "A mode for diaspora* stream view and posting."
  :group 'applications)

;;; User variable:
(defcustom diaspora-secure-pod
  t
  "If your diaspora pod use https, set this to true.
If only use http, use false."
  :type 'boolean
  :group 'diaspora)

(defcustom diaspora-pod 
  "joindiaspora.com"
  "Your diaspora* pod."
  :type 'string
  :group 'diaspora)

(defcustom diaspora-posts-directory
  "~/.diaspora/posts/"
  "Diaspora* temp dir (abs path)."
  :type 'dir
  :group 'diaspora)


(defcustom diaspora-temp-directory
  "~/.diaspora/temp/"
  "Diaspora* temp dir (abs path)."
  :type 'dir
  :group 'diaspora)

(defcustom diaspora-image-directory
  "~/.diaspora/img/"
  "Diaspora* image dir (abs path)."
  :type 'dir
  :group 'diaspora)


(defcustom diaspora-show-user-avatar t
   "Show user images beside each users entry."
   :type 'boolean
   :group 'diaspora)

(defcustom diaspora-mode-hook nil
  "Functions run upon entering `diaspora-mode'."
  :type 'hook
  :options '(flyspell-mode turn-on-auto-fill longlines-mode diaspora-get-all-images diaspora-show-images)
  :group 'diaspora)

(defcustom diaspora-username nil
  "Username to use for connecting to diaspora.
If nil, you will be prompted."
  :type 'string
  :group 'diaspora)

(defcustom diaspora-password nil
  "Password to use for connecting to diaspora.
If nil, you will be prompted."
  :type 'string
  :group 'diaspora)

(defcustom diaspora-sign-in-url 
  "https://joindiaspora.com/users/sign_in"
  "URL used to signing in."
  :group 'diaspora)

(defcustom diaspora-status-messages-url 
  "https://joindiaspora.com/status_messages"
  "URL used to update diaspora status messages."
  :group 'diaspora)

(defcustom diaspora-single-message-url
  "https://joindiaspora.com/posts"
  "URL used to get a single message.")

(defcustom diaspora-entry-stream-url 
  "https://joindiaspora.com/explore.json"
  "JSON version of the entry stream(the main stream)."
  :group 'diaspora)

(defcustom diaspora-entry-likes-url 
  "https://joindiaspora.com/participate.json"
  "JSON version of the entry stream(the main stream)."
  :group 'diaspora)


(defvar diaspora-notifications-url "https://joindiaspora.com/notifications.json"
  "This is the URL for JSON format notifications.")


(defcustom diaspora-entry-file-dir
  "~/public_html/diaspora.posts/"
  "Directory where to save posts made to diaspora*."
  :group 'diaspora)

;; (defcustom diaspora-data-file
;;   "~/.diaspora"
;;   "Name of the file do save posts made to diaspora*."
;;   :type 'file
;;   :group 'diaspora)

(defcustom diaspora-data-directory
  "~/.diaspora/"
  "Directory where for saving."
  :type 'file
  :group 'diaspora)


(defcustom diaspora-header-post
  "### "
  "Header for each post:"
  :type 'string
  :group 'diaspora)

(defcustom diaspora-footer-post
  "#diaspora-el"
  "Footer for each post."
  :type 'string
  :group 'diaspora)

(defcustom diaspora-save-after-posting t
  "*Non-nil means automatically save after posting."
  :type 'boolean
  :group 'diaspora)


(defcustom diaspora-stream-register ?R
  "The register in which the window configuration is stored."
  :type 'character
  :group 'diaspora)


(defcustom diaspora-post-register ?R
  "The register in which the window configuration is stored."
  :type 'character
  :group 'diaspora)

(defcustom diaspora-single-message-register ?R
  "The register in which the window configuration is stored."
  :type 'character
  :group 'diaspora)

(defcustom diaspora-save-after-posting t
  "*Non-nil means automatically save after posting."
  :type 'boolean
  :group 'diaspora)

;;; Internal Variables:


(defvar diaspora-auth-token nil
  "Authenticity token variable name.")


(defvar diaspora-stream-buffer "*diaspora stream*"
  "The name of the diaspora stream buffer.")

(defvar diaspora-post-buffer "*diaspora post*"
  "The name of the diaspora post buffer.")

(defvar diaspora-single-message-buffer "*diaspora message*"
  "The name of the diaspora single message buffer.")

(defvar  diaspora-stream-tag-buffer
  "*diaspora stream tag*"
  "The name of the diaspora tag stream buffer.")

(defvar diaspora-notifications-buffer
  "*diaspora notifications*"
    "The name of the diaspora notifications buffer.")


(defcustom diaspora-image-external-program "eog"
  "This is the program path and name. If you want to see an image in an external program this must be
setted correctly."
  :group 'diaspora
  :type 'string)

;;; User Functions:

(defun diaspora ()
  "Make all dirs if they don' exist and set `diaspora-username' 
and  `diaspora-password' no matter what.  
To be called interactively instead of `diaspora-ask'"
  (interactive)
  (diaspora-make-dirs)
  (diaspora-ask t))
  
(defun diaspora-make-dirs ()
  "Make all dirs if they don' exist."
  (unless (file-exists-p diaspora-data-directory)    
    (make-directory diaspora-data-directory))
  (unless (file-exists-p diaspora-temp-directory)
    (make-directory diaspora-temp-directory))
  (unless (file-exists-p diaspora-posts-directory)
    (make-directory diaspora-posts-directory))
  (unless (file-exists-p diaspora-image-directory)
    (make-directory diaspora-image-directory)))

(defun diaspora-clean-cache ()
  (interactive)
  (shell-command (concat "rm -f " diaspora-image-directory "*")))


(defun diaspora-ask (&optional opt)
  "Ask for username and password if `diaspora-username' 
and  `diaspora-password' has not been setted. `opt' t forces setting."
  (unless (and diaspora-username diaspora-password (null opt))
      ;; Diaspora username and password was not setted.
    (list
     (setq diaspora-username (read-string "username: "
					  diaspora-username
					  nil nil))
     (setq diaspora-password (read-passwd "password: ")))))





(defun diaspora-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "diaspora.el, version %s" diaspora-el-version))

;;;###autoload
(define-derived-mode diaspora-mode text-mode "diaspora"
  "Major mode for output from \\[diaspora*]."
  (set (make-local-variable 'font-lock-defaults)
       '(diaspora-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t)
  (use-local-map diaspora-mode-map)
  (run-hooks 'diaspora-mode-hook))



(add-hook 'diaspora-mode-hook 'diaspora-get-all-images)
(add-hook 'diaspora-mode-hook 'diaspora-show-images)
(add-hook 'diaspora-mode-hook 'diaspora-see-regexp-markdow)
(add-hook 'diaspora-mode-hook 'diaspora-show-videos)



(defun diaspora-image-path (image-name)
  "Return the temporal image path."
  (concat diaspora-image-directory image-name)
  )

(provide 'diaspora)

;;; diaspora.el ends here.

