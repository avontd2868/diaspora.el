;;; diaspora-stream-mode.el --- 
;; 
;; Filename: diaspora-stream-mode.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: mié may  2 10:00:09 2012 (-0300)
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
;; Minor mode for view streams. 
;; 
;; Just gives some keymaps, and the hability to make the buffer read-only.
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


(defvar diaspora-stream-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cl" 'diaspora-get-and-show-images)
    (define-key map "\C-cio" 'diaspora-show-image-at-point)
    (define-key map "q" 'kill-buffer)
    (define-key map "g" 'diaspora-visit-last-stream)
    (define-key map [tab] 'diaspora-stream-mode-next-element)
    map
    )
  )

(define-minor-mode diaspora-stream-mode 
  "Minor mode for viewing Diaspora's streams. Use it only for this PURPOSE.

This minor mode applies a couple of properties in the current buffer, so it can't be used with other minor modes.
It is intended to be used only with `diaspora-mode' major mode and no other minor mode."
  nil
  " D*-stream"
  diaspora-stream-mode-map
  :group 'diaspora
  
  (set (make-local-variable 'buffer-read-only) t)

  (if diaspora-stream-mode
      (diaspora-hide-markdown)
    (diaspora-show-markdown)
    )
  )

(defun diaspora-stream-mode-next-element (&rest r)
  "Go to the next interactive element in the current buffer."
  (interactive)
  (diaspora-misc-next-option 'keymap)
  )

(provide 'diaspora-stream-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diaspora-stream-mode.el ends here
