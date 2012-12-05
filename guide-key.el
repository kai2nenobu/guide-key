;;; guide-key.el --- Guide the following key bindings automatically and dynamically

;; Copyright (C) 2012 Tsunenobu Kai

;; Author: Tsunenobu Kai <kbkbkbkb1@gmail.com>
;; URL: https://github.com/kbkbkbkb1/guide-key
;; Version: 1.0.1
;; Package-Requires: ((popwin "0.3.0"))
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

;; Overview:
;;
;; guide-key.el enables to guide the following key bindings automatically and
;; dynamically. guide-key aims to be an alternative of one-key.el.
;;
;; Here are some features of this library.
;; - guide-key automatically pop up the keys following your favorite
;;   prefixes. Moreover, even if you change key bindings, guide-key follows its
;;   change dynamically.
;; - guide-key can highlight particular commands. This makes it easy to find a
;;   command you are looking for, and to learn its key binding.
;; - guide-key doesn't overwrite existing commands and key bindings. So, there
;;   is no bad effect on using `describe-key' and `describe-bindings'.
;;
;; Installation:
;;
;; I added guide-key to MELPA. You can install guide-key with package.el,
;; Because guide-key depends on popwin.el, popwin.el is also installed at a
;; time.
;;
;; If you don't have package.el, please download popwin.el and guide-key.el
;; directly from https://github.com/m2ym/popwin-el and
;; https://github.com/kbkbkbkb1/guide-key, and then put them in your
;; `load-path'.
;;
;; Usage:
;;
;; You just add your favorite prefix keys to `guide-key/guide-key-sequence'
;; as below.
;;
;;   (require 'guide-key)
;;   (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
;;   (guide-key-mode 1) ; Enable guide-key-mode
;;
;; When you press these "C-x r" or "C-x 4", key bindings are automatically
;; popped up.
;;
;; guide-key can highlight commands which match a specified regular
;; expression.  Key bindings following "C-x r" are rectangle family and
;; register family.  If you want to highlight only rectangle family commands,
;; put this setting in your init.el.
;;
;;   (setq guide-key/highlight-command-regexp "rectangle")
;;
;; This feature makes it easy to find commands and learn their key bindings. If
;; you want to highlight both rectangle family and register family, set an
;; adequate regular expression like this.
;;
;;   (setq guide-key/highlight-command-regexp "rectangle\\|register")
;;
;; Moreover, prefix commands are automatically highlighted.
;;
;; Here are some functions and variables which control guide-key.
;; - `guide-key-mode':
;;    guide-key-mode is implemented as a minor mode.
;;    Excuting M-x guide-key-mode toggles whether guide-key is enabled or
;;    not.  Because guide-key-mode is a global minor mode, guide-key-mode is
;;    enabled in all buffers or disabled in all buffers.
;; - `guide-key/popup-window-position':
;;   This variable controls where a guide-key buffer is popped up. A value of
;;   this variable is one of `right', `bottom', `left', `top'. The default
;;   value is `right'.
;; - `guide-key/polling-time':
;;    This variable controls a polling time. The default value is 0.1 (in seconds).
;;
;; I've confirmed that guide-key works well in these environments.
;; - Emacs 24.2, Ubuntu 12.04 or Windows 7 64bit
;; - Emacs 23.3, Ubuntu 12.04 or Windows 7 64bit
;; - Emacs 22.3, Windows 7 64bit
;; If popwin works good, I think guide-key also works good. You can use
;; guide-key with Emacs working in terminal.
;;
;; You can add extra settings in a particular mode. Please use
;; `guide-key/add-local-guide-key-sequence',
;; `guide-key/add-local-highlight-command-regexp' and hook of
;; that mode.
;;
;; This code is a example of org-mode.
;;
;;   (defun guide-key/my-hook-function-for-org-mode ()
;;     (guide-key/add-local-guide-key-sequence "C-c")
;;     (guide-key/add-local-guide-key-sequence "C-c C-x")
;;     (guide-key/add-local-highlight-command-regexp "org-"))
;;   (add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)
;;
;;
;; Enjoy!

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

;; - Version 1.0.1
;;   - Change to save and restore a last config of popwin
;; - Version 1.0.0
;;   - First release version
;;   - Adjust names of functions and variables
;;   - Add some documentations
;; - Version 0.1.2
;;   - Enable to guide key-chord bindings.
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
  :prefix "guide-key/")

(defcustom guide-key/guide-key-sequence nil
  "*Key sequences to guide in `guide-key-mode'.
This variable is a list of string representation.
Both representations, like \"C-x r\" and \"\\C-xr\",
are allowed."
  :type '(repeat string)
  :group 'guide-key)

(defcustom guide-key/polling-time 0.1
  "*Polling time to check an input key sequence."
  :type 'float
  :group 'guide-key)

(defcustom guide-key/highlight-prefix-regexp "prefix"
  "*Regexp for prefix commands."
  :type 'regexp
  :group 'guide-key)

(defcustom guide-key/highlight-command-regexp ""
  "*Regexp for commands to highlight."
  :type 'regexp
  :group 'guide-key)

(defcustom guide-key/align-command-by-space-flag nil
  "*If non-nil, align guide buffer by space."
  :type 'boolean
  :group 'guide-key)

(defcustom guide-key/popup-window-position 'right
  "*Position where guide buffer is popped up.
This variable must be one of `right', `bottom', `left' and `top'."
  :type '(radio (const right) (const bottom) (const left) (const top))
  :group 'guide-key)

(defface guide-key/prefix-command-face
  '((((class color) (background dark))
     (:foreground "cyan"))
    (((class color) (background light))
     (:foreground "blue")))
  "Face for prefix commands to highlight"
  :group 'guide-key)

(defface guide-key/highlight-command-face
  '((((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "orange red")))
  "Face for commands to highlight"
  :group 'guide-key)

(defface guide-key/key-face
  '((((class color) (background dark))
     (:foreground "red"))
    (((class color) (background light))
     (:foreground "dark green")))
  "Face for keys following to a key sequence"
  :group 'guide-key)

;;; internal variables
(defvar guide-key/polling-timer nil
  "Polling timer to check an input key sequence.")

(defvar guide-key/guide-buffer-name " *guide-key*"
  "Buffer name of guide buffer.")

(defvar guide-key/last-key-sequence-vector nil
  "Key sequence input at the last polling operation.")

;; or hook
;; (add-hook 'pre-command-hook 'guide-key/hook-command)
;; (setq pre-command-hook nil)
;; (add-hook 'post-command-hook 'guide-key/key-event)
;; (add-hook 'pre-command-hook 'show-this-command)

;;; functions
;;;###autoload
(define-minor-mode guide-key-mode
  "Toggle guide key mode.

In guide key mode, Guide following keys to an input key sequence
automatically and dynamically.
With a prefix argument ARG, enable guide key mode if ARG is
positive, otherwise disable."
  :global t
  :lighter " Guide"
  (funcall (if guide-key-mode
               'guide-key/turn-on-timer
             'guide-key/turn-off-timer)))

;;; internal functions
(defun guide-key/polling-function ()
  "Polling function executed every `guide-key/polling-time' second."
  (let ((key-seq (this-command-keys-vector)))
    (if (guide-key/popup-guide-buffer-p key-seq)
        (when (guide-key/update-guide-buffer-p key-seq)
          (let ((dsc-buf (current-buffer))
                (hi-regexp guide-key/highlight-command-regexp)
                (max-width 0))
            (with-current-buffer (get-buffer-create guide-key/guide-buffer-name)
              (unless truncate-lines (setq truncate-lines t))   ; don't fold line
              (when indent-tabs-mode (setq indent-tabs-mode nil)) ; don't use tab as white space
              (erase-buffer)
              (describe-buffer-bindings dsc-buf key-seq)
              (if (> (guide-key/format-guide-buffer key-seq hi-regexp) 0)
                  (progn
                    (guide-key/close-guide-buffer)
                    (guide-key/popup-guide-buffer))
                (message "No following key.")))))
      (guide-key/close-guide-buffer))
    (setq guide-key/last-key-sequence-vector key-seq)))

(defun guide-key/popup-guide-buffer ()
  "Pop up guide buffer at `guide-key/popup-window-position'."
  (let ((last-config popwin:popup-last-config))
    (with-current-buffer (get-buffer guide-key/guide-buffer-name)
      (apply 'popwin:popup-buffer (current-buffer)
             :position guide-key/popup-window-position
             :noselect t
             (cond ((popwin:position-horizontal-p guide-key/popup-window-position)
                    `(:width ,(+ (guide-key/buffer-max-width) 3)))
                   ((popwin:position-vertical-p guide-key/popup-window-position)
                    `(:height ,(+ (count-lines (point-min) (point-max)) 3))))))
    (setq popwin:popup-last-config last-config)))

(defun guide-key/close-guide-buffer ()
  "Close guide buffer."
  (when (eq popwin:popup-buffer (get-buffer guide-key/guide-buffer-name))
    (popwin:close-popup-window)))

(add-hook 'pre-command-hook 'guide-key/close-guide-buffer)

(defun guide-key/update-guide-buffer-p (key-seq)
  "Return t if guide buffer should be updated."
  (not (equal guide-key/last-key-sequence-vector key-seq)))

(defun guide-key/popup-guide-buffer-p (key-seq)
  "Return t if guide buffer should be popped up."
  (and (> (length key-seq) 0)
       (member key-seq (mapcar 'guide-key/convert-key-sequence-to-vector
                               guide-key/guide-key-sequence))))

(defun guide-key/convert-key-sequence-to-vector (key-seq)
  "Convert key sequence KEY-SEQ to vector representation.
For example, both \"C-x r\" and \"\\C-xr\" are converted to [24 114]"
  (vconcat (read-kbd-macro key-seq)))

(defun guide-key/turn-on-timer ()
  "Turn on a polling timer."
  (when (null guide-key/polling-timer)
    (setq guide-key/polling-timer
          (run-at-time t guide-key/polling-time 'guide-key/polling-function))))

(defun guide-key/turn-off-timer ()
  "Turn off a polling timer."
  (cancel-timer guide-key/polling-timer)
  (setq guide-key/polling-timer nil))

(defun guide-key/format-guide-buffer (key-seq hi-regexp)
  "Format guide buffer. This function returns the number of following keys."
  (let ((fkey-list nil)      ; list of (following-key space command)
        (fkey-str-list nil)  ; fontified string of `fkey-list'
        (fkey-list-len 0)    ; length of above lists
        (key-dsc (key-description key-seq)))
    (untabify (point-min) (point-max))  ; replace tab to space
    (goto-char (point-min))
    ;; extract following keys from buffer bindings
    (while (re-search-forward
            (format "^%s \\([^ \t]+\\)\\([ \t]+\\)\\(\\(?:[^ \t\n]+ ?\\)+\\)$" key-dsc) nil t)
      (add-to-list 'fkey-list
                   (list (match-string 1) (match-string 2) (match-string 3)) t))
    (erase-buffer)
    (when (> (setq fkey-list-len (length fkey-list)) 0)
      ;; fontify following keys as string
      (setq fkey-str-list
            (loop for (key space command) in fkey-list
                  collect (guide-key/fontified-string key space command hi-regexp)))
      ;; insert a few following keys per line
      (cond ((popwin:position-horizontal-p guide-key/popup-window-position)
             (guide-key/insert-following-key
              fkey-str-list (1+ (/ (length fkey-str-list) (1- (frame-height))))))
            ((popwin:position-vertical-p guide-key/popup-window-position)
             (guide-key/insert-following-key  ; caluculation of second argument is rough
              fkey-str-list (/ (frame-width)
                                (apply 'max (mapcar 'length fkey-str-list))))))
      (align-regexp (point-min) (point-max) "\\(\\s-*\\) \\[" 1 1 t)
      (goto-char (point-min)))
    fkey-list-len))

(defun guide-key/insert-following-key (fkey-str-list columns)
  "Insert following keys by COLUMNS per line."
  (loop for fkey-str in fkey-str-list
        for column from 1
        do (insert fkey-str (if (= (mod column columns) 0) "\n" " "))))

(defun guide-key/fontified-string (key space command hi-regexp)
  "Return fontified string of following key"
  (concat (propertize "[" 'face 'guide-key/key-face)
          (guide-key/propertize-string-according-to-command key command hi-regexp)
          (propertize "]" 'face 'guide-key/key-face)
          (if guide-key/align-command-by-space-flag space " ") ; white space
          (guide-key/propertize-string-according-to-command command command hi-regexp)))

(defun guide-key/propertize-string-according-to-command (string command hi-regexp)
  "Return STRING putted text property accordinig to COMMAND"
  (cond ((string-match guide-key/highlight-prefix-regexp command)
         (propertize string 'face 'guide-key/prefix-command-face))
        ((and (not (string= hi-regexp ""))
              (string-match hi-regexp command))
         (propertize string 'face 'guide-key/highlight-command-face))
        (t
         string)))

(defun guide-key/buffer-max-width ()
  "Return max width in current buffer."
  (let ((buf-str (buffer-substring-no-properties (point-min) (point-max))))
    (apply 'max (mapcar 'length (split-string buf-str "\n")))))

(defun guide-key/add-local-guide-key-sequence (key)
  (add-to-list (make-local-variable 'guide-key/guide-key-sequence) key))

(defun guide-key/add-local-highlight-command-regexp (regexp)
  (set (make-local-variable 'guide-key/highlight-command-regexp)
       (if (string= guide-key/highlight-command-regexp "")
           regexp
         (concat regexp "\\|" guide-key/highlight-command-regexp))))

;;; key-chord hack
(defadvice this-command-keys (after key-chord-hack disable)
  ""
  (condition-case nil
      (if (equal ad-return-value [key-chord])
          (let ((rkeys (recent-keys)))
            (setq ad-return-value
                  (vector 'key-chord (aref rkeys (- (length rkeys) 2))
                          (aref rkeys (- (length rkeys) 1))))))
    (error "")))

(defadvice this-command-keys-vector (after key-chord-hack disable)
  ""
  (condition-case nil
      (if (equal ad-return-value [key-chord])
          (let ((rkeys (recent-keys)))
            (setq ad-return-value
                  (vector 'key-chord (aref rkeys (- (length rkeys) 2))
                          (aref rkeys (- (length rkeys) 1))))))
    (error [])))

(defun guide-key/key-chord-hack-on ()
  "Turn on key-chord hack of guide-key."
  (interactive)
  (dolist (fn '(this-command-keys this-command-keys-vector))
    (ad-enable-advice fn 'after 'key-chord-hack)
    (ad-activate fn))
  (message "Turn on key-chord hack of guide-key"))

(defun guide-key/key-chord-hack-off ()
  "Turn off key-chord hack of guide-key."
  (interactive)
  (dolist (fn '(this-command-keys this-command-keys-vector))
    (ad-disable-advice fn 'after 'key-chord-hack)
    (ad-activate fn))
  (message "Turn on key-chord hack of guide-key"))

;;; debug
(defun guide-key/message-events ()
  ""
  (message (format "lce:%S tck:%S tckv:%S lie:%S uce:%S"
                   last-command-event
                   (this-command-keys)
                   (this-command-keys-vector)
                   last-input-event
                   unread-command-events
                   )))
;; (setq ttt (run-at-time t 1 'guide-key/message-events))
;; (cancel-timer ttt)

(provide 'guide-key)
;;; guide-key.el ends here
