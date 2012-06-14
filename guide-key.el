;;; guide-key.el --- Guide key bindings.

;; Copyright (C) 2012 Tsunenobu Kai

;; Author: Tsunenobu Kai <kbkbkbkb1@gmail.com>
;; Version: 0.1.1
;; Keywords: help convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Write later

;;; TODO:
;; - write document
;; - DONE? enrichment of guide buffer
;;   - select color to be used
;; - DONE automatically guide all following keys
;; - DONE pop up guide buffer at top or bottom
;; - cooperate with polling of popwin.el?
;; - confine a length of command name
;; - confine the number of items to guide
;; - a feature to exclude or include guide by command name
;; - DONE define (buffer local) minor mode instead of global minor mode
;; - prefix argument processing
;; - DONE define global minor mode

;;; ChangeLog:
;; - Version 0.1.1
;;   - Make `guide-key-mode' global minor mode.
;; - Version 0.1.0
;;   - Initial version.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'popwin)

;;; variables
(defgroup guide-key nil
  "Guide key bidings."
  :group 'help
  :prefix "guide-key:")

(defcustom guide-key:show-key-sequence nil
  "*Key sequences to show its bindings."
  :type '(repeat string)
  :group 'guide-key)

(defcustom guide-key:polling-time 0.1
  "*Polling time to show bindings."
  :type 'float
  :group 'guide-key)

(defcustom guide-key:highlight-prefix-regexp "prefix"
  "*Regexp of prefix command to highlight."
  :type 'regexp
  :group 'guide-key)

(defcustom guide-key:highlight-command-regexp ""
  "*Regexp of command to highlight."
  :type 'regexp
  :group 'guide-key)

(defcustom guide-key:align-command-by-space-flag nil
  "*If non-nil, align command by space."
  :type 'boolean
  :group 'guide-key)

(defcustom guide-key:popup-window-position 'bottom
  "*Position to pop up buffer. This variable must be one of `right', `bottom', `left' and `top'."
  :type 'symbol
  :group 'guide-key)

(defface guide-key:prefix-command-face
  '((((class color) (background dark))
     (:foreground "cyan"))
    (((class color) (background light))
     (:foreground "blue")))
  "Face for prefix command"
  :group 'guide-key)

(defface guide-key:highlight-command-face
  '((((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "orange red")))
  "Face for command to highlight"
  :group 'guide-key)

(defface guide-key:key-face
  '((((class color) (background dark))
     (:foreground "red"))
    (((class color) (background light))
     (:foreground "dark green")))
  "Face for key"
  :group 'guide-key)

;;; internal variables
;; (defvar guide-key:guide-list nil
;;   "List of key guide. Element of this is like (KEY SPACE COMMAND).")

(defvar guide-key:polling-timer nil
  "Polling timer for show bindings.")

(defvar guide-key:buffer-name " *guide-key*"
  "Buffer name to show bindings.")

(defvar guide-key:last-command-keys-vector nil
  "Last command keys as vector.")

;; or hook
;; (add-hook 'pre-command-hook 'guide-key:hook-command)
;; (setq pre-command-hook nil)
;; (add-hook 'post-command-hook 'guide-key:key-event)
;; (add-hook 'pre-command-hook 'show-this-command)

;;; functions
;;;###autoload
(define-minor-mode guide-key-mode
  "Show bindings automatically."
  :global t
  :lighter " Guide"
  (funcall (if guide-key-mode
               'guide-key:turn-on-timer
             'guide-key:turn-off-timer)))

;;;###autoload
;; (define-globalized-minor-mode global-guide-key-mode guide-key-mode guide-key-on)

(defun guide-key-on ()
  (unless (minibufferp)
    (guide-key-mode 1)))

;;; internal functions
(defun guide-key:polling-timer-function ()
  "Function executed every `guide-key:polling-time' second."
  (when guide-key-mode
    (let ((dsc-buf (current-buffer))
          (hi-regexp guide-key:highlight-command-regexp)
          (key-seq (this-command-keys-vector))
          (max-width 0))
      (if (guide-key:display-popup-p key-seq)
          (when (guide-key:update-popup-p key-seq)
            (with-current-buffer (get-buffer-create guide-key:buffer-name)
              (unless truncate-lines (setq truncate-lines t))   ; don't fold
              (when indent-tabs-mode (setq indent-tabs-mode nil)) ; don't use tab as white space
              (erase-buffer)
              (describe-buffer-bindings dsc-buf key-seq)
              (if (> (guide-key:format-guide-buffer key-seq hi-regexp) 0)
                  (progn
                    (guide-key:pre-command-popup-close)
                    (guide-key:popup-guide-buffer))
                (message "No following key."))))
        (guide-key:pre-command-popup-close))
      (setq guide-key:last-command-keys-vector key-seq))))

(defun guide-key:popup-guide-buffer ()
  "Pop up guide buffer."
  (with-current-buffer (get-buffer guide-key:buffer-name)
    (apply 'popwin:popup-buffer (current-buffer)
           :position guide-key:popup-window-position
           :noselect t
           (cond ((popwin:position-horizontal-p guide-key:popup-window-position)
                  `(:width ,(+ (guide-key:buffer-max-width) 3)))
                 ((popwin:position-vertical-p guide-key:popup-window-position)
                  `(:height ,(+ (count-lines (point-min) (point-max)) 3)))))
           ;; (case guide-key:popup-window-position
           ;;   (right `(:width ,(+ (guide-key:buffer-max-width) 1)))
           ;;   (bottom `(:height ,(+ (count-lines (point-min) (point-max)) 3)))
           ;;   (left `(:width ,(+ (guide-key:buffer-max-width) 3)))
           ;;   (top `(:height ,(+ (count-lines (point-min) (point-max)) 3)))))
    ))

(defun guide-key:pre-command-popup-close ()
  "Close guide buffer at `pre-command-hook'."
  (when (guide-key:poppedup-p)
    (popwin:close-popup-window)))

(add-hook 'pre-command-hook 'guide-key:pre-command-popup-close)

(defun guide-key:update-popup-p (key-seq)
  "Return t if show bindings buffer should be updated."
  (not (equal guide-key:last-command-keys-vector key-seq)))

(defun guide-key:display-popup-p (key-seq)
  "Return t if show bindings buffer should be displayed."
;  (and (> (length key-seq) 0)
       (member key-seq (mapcar 'guide-key:convert-key-sequence-to-vector
                               guide-key:show-key-sequence))
       );)

(defun guide-key:convert-key-sequence-to-vector (key-seq)
  "Convert key sequence KEY-SEQ to vector representation."
  (vconcat (read-kbd-macro key-seq)))

(defun guide-key:poppedup-p ()
  "Return t if show bindings buffer is popped up."
  (eq popwin:popup-buffer (get-buffer guide-key:buffer-name)))

(defun guide-key:turn-on-timer ()
  "Turn on polling timer."
  (when (null guide-key:polling-timer)
    (setq guide-key:polling-timer
          (run-at-time t guide-key:polling-time 'guide-key:polling-timer-function))))

(defun guide-key:turn-off-timer ()
  "Turn off polling timer."
  (cancel-timer guide-key:polling-timer)
  (setq guide-key:polling-timer nil))

(defun guide-key:format-guide-buffer (key-seq hi-regexp)
  "Format a guide buffer. This function returns the number of key guides."
  (let ((guide-list nil)      ; list of (key space command)
        (guide-str-list nil)  ; list of fontified string of key guides
        (guide-list-len 0)    ; length of above lists
        (key-dsc (key-description key-seq)))
    (untabify (point-min) (point-max))  ; replace tab to space
    (goto-char (point-min))
    ;; extract key guide from buffer bindings
    (while (re-search-forward
            (format "^%s \\([^ \t]+\\)\\([ \t]+\\)\\(\\(?:[^ \t\n]+ ?\\)+\\)$" key-dsc) nil t)
      (add-to-list 'guide-list
                   (list (match-string 1) (match-string 2) (match-string 3)) t))
    (erase-buffer)
    (when (> (setq guide-list-len (length guide-list)) 0)
      ;; fontify key guide string
      (setq guide-str-list
            (loop for (key space command) in guide-list
                  collect (guide-key:fontified-string key space command hi-regexp)))
      ;; insert a few strings per line
      (cond ((popwin:position-horizontal-p guide-key:popup-window-position)
             (guide-key:insert-guide-str-list
              guide-str-list (1+ (/ (length guide-str-list) (1- (frame-height))))))
            ((popwin:position-vertical-p guide-key:popup-window-position)
             (guide-key:insert-guide-str-list  ; caluculation of second argument is rough
              guide-str-list (/ (frame-width)
                                (apply 'max (mapcar 'length guide-str-list))))))
      (align-regexp (point-min) (point-max) "\\(\\s-*\\) \\[" 1 1 t)
      (goto-char (point-min)))
    guide-list-len))

(defun guide-key:insert-guide-str-list (guide-str-list columns)
  "Insert GUIDE-STR-LIST COLUMNS."
  (loop for guide-str in guide-str-list
        for column from 1
        do (insert guide-str (if (= (mod column columns) 0) "\n" " "))))

(defun guide-key:fontified-string (key space command hi-regexp)
  "Fontified string for key guide"
  (concat (propertize "[" 'face 'guide-key:key-face)
          (guide-key:propertize-string-according-to-command key command hi-regexp)
          (propertize "]" 'face 'guide-key:key-face)
          (if guide-key:align-command-by-space-flag space " ") ; white space
          (guide-key:propertize-string-according-to-command command command hi-regexp)))

(defun guide-key:propertize-string-according-to-command (string command hi-regexp)
  "Return STRING putted text property accordinig to COMMAND"
  (cond ((string-match guide-key:highlight-prefix-regexp command)
         (propertize string 'face 'guide-key:prefix-command-face))
        ((and (not (string= hi-regexp ""))
              (string-match hi-regexp command))
         (propertize string 'face 'guide-key:highlight-command-face))
        (t
         string)))

(defun guide-key:buffer-max-width ()
  "Return max width in current buffer."
  (let ((buf-str (buffer-substring-no-properties (point-min) (point-max))))
    (apply 'max (mapcar 'length (split-string buf-str "\n")))))

(defun guide-key:add-local-show-key-sequence (key)
  (add-to-list (make-local-variable 'guide-key:show-key-sequence) key))

(defun guide-key:add-local-highlight-command-regexp (regexp)
  (set (make-local-variable 'guide-key:highlight-command-regexp)
       (if (string= guide-key:highlight-command-regexp "")
           regexp
         (concat regexp "\\|" guide-key:highlight-command-regexp))))

;;; key-chord hack
(defadvice this-command-keys (after my-key-chord-hack activate)
  ""
  (condition-case nil
      (if (equal ad-return-value [key-chord])
          (let ((rkeys (recent-keys)))
            (setq ad-return-value
                  (vector 'key-chord (aref rkeys (- (length rkeys) 2))
                          (aref rkeys (- (length rkeys) 1))))))
    (error "")))

(defadvice this-command-keys-vector (after my-key-chord-hack activate)
  ""
  (condition-case nil
      (if (equal ad-return-value [key-chord])
          (let ((rkeys (recent-keys)))
            (setq ad-return-value
                  (vector 'key-chord (aref rkeys (- (length rkeys) 2))
                          (aref rkeys (- (length rkeys) 1))))))
    (error [])))

;;; debug
(defun guide-key:message-events ()
  ""
  (message (format "lce:%S tck:%S tckv:%S lie:%S uce:%S popb:%S cls:%S"
                   last-command-event
                   (this-command-keys)
                   (this-command-keys-vector)
                   last-input-event
                   unread-command-events
                   popwin:popup-buffer
                   (guide-key:poppedup-p)
                   )))
;; (setq ttt (run-at-time t 1 'guide-key:message-events))
;; (cancel-timer ttt)

(provide 'guide-key)
;;; guide-key.el ends here
