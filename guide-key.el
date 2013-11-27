;;; guide-key.el --- Guide the following key bindings automatically and dynamically

;; Copyright (C) 2012, 2013 Tsunenobu Kai

;; Author: Tsunenobu Kai <kbkbkbkb1@gmail.com>
;; URL: https://github.com/kbkbkbkb1/guide-key
;; Version: 1.2.0
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
;; guide-key.el displays the available key bindings automatically and dynamically.
;; guide-key aims to be an alternative of one-key.el.
;;
;; Here are some features of this library.
;; - guide-key automatically pops up the keys following your favorite
;;   prefixes. Moreover, even if you change key bindings, guide-key follows the
;;   change dynamically.
;; - guide-key can highlight particular commands. This makes it easy to find a
;;   command you are looking for, and to learn its key binding.
;; - guide-key doesn't overwrite existing commands and key bindings, so there
;;   is no interference with `describe-key' and `describe-bindings'.
;;
;;
;; Installation:
;;
;; I added guide-key to MELPA. You can install guide-key with package.el.
;; Because guide-key depends on popwin.el, popwin.el is also installed.
;;
;; If you don't have package.el, please download popwin.el and guide-key.el
;; directly from https://github.com/m2ym/popwin-el and
;; https://github.com/kbkbkbkb1/guide-key, and then put them in your
;; `load-path'.
;;
;;
;; Basic usage:
;;
;; You just add your favorite prefix keys to `guide-key/guide-key-sequence'
;; as below.
;;
;;   (require 'guide-key)
;;   (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
;;   (guide-key-mode 1) ; Enable guide-key-mode
;;
;; When you press these prefix keys, key bindings are automatically
;; popped up after a short delay (1 second by default).
;;
;; guide-key can highlight commands which match a specified regular expression.
;; Key bindings following "C-x r" are rectangle family and register family.
;; If you want to highlight only rectangle family commands, put this setting
;; in your init.el.
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
;; Depending on your level of emacs experience, you may want a shorter or
;; longer delay between pressing a key and the appearance of the guide
;; buffer.  This can be controlled by setting `guide-key/idle-delay':
;;
;;   (setq guide-key/idle-delay 0.1)
;;
;; The guide buffer is displayed only when you pause between keystrokes
;; for longer than this delay, so it will keep out of your way when you
;; are typing key sequences that you already know well.
;;
;; I've confirmed that guide-key works well in these environments.
;; - Emacs 24.2, Ubuntu 12.04 or Windows 7 64bit
;; - Emacs 23.3, Ubuntu 12.04 or Windows 7 64bit
;; - Emacs 22.3, Windows 7 64bit
;; - Emacs 24.3.1, OS X 10.9
;; If popwin works, I think guide-key will work as well. You can use
;; guide-key with Emacs working in terminal.
;;
;;
;; Advanced usage:
;;
;; It is bothering to add many prefixes to `guide-key/guide-key-sequence'.
;; `guide-key/recursive-key-sequence-flag' releases you from this problem.
;; If `guide-key/recursive-key-sequence-flag' is non-nil, guide-key checks a
;; input key sequence recursively. That is, if "C-x 8 ^" is an input key
;; sequence, guide-key checks whether `guide-key/guide-key-sequence' includes
;; "C-x 8" and "C-x".
;;
;; For example, if you configure as below,
;;
;;   (setq guide-key/guide-key-sequence '("C-x"))
;;   (setq guide-key/recursive-key-sequence-flag t)
;;
;; the guide buffer is popped up when you input "C-x r", "C-x 8" and
;; any other prefixes following "C-x".
;;
;;
;; You can add extra settings in a particular mode. Please use
;; `guide-key/add-local-guide-key-sequence',
;; `guide-key/add-local-highlight-command-regexp' and the hook of
;; that mode.
;;
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
;; Here are some functions and variables which control guide-key.
;; - `guide-key-mode':
;;   guide-key-mode is implemented as a minor mode.
;;   Excuting M-x guide-key-mode toggles whether guide-key is enabled or
;;   not.  Because guide-key-mode is a global minor mode, guide-key-mode is
;;   enabled in all buffers or disabled in all buffers.
;; - `guide-key/popup-window-position':
;;   This variable controls where a guide-key buffer is popped up. A value of
;;   this variable is one of `right', `bottom', `left', `top'. The default
;;   value is `right'.
;; - `guide-key/polling-time':
;;   This variable controls a polling time. The default value is 0.1 (in seconds).
;; - `guide-key/idle-delay':
;;   This variable controls the delay between starting a key sequence and
;;   popping up the guide buffer. The default value is 1.0 (in seconds),
;;   which means that guide-key will keep out of your way unless you hesitate
;;   in the middle of a key sequence .  Set this to 0.0 to revert to the old
;;   default behavior.
;; - `guide-key/text-scale-amount':
;;   This variable controls the size of text in guide buffer. The default
;;   value is 0 (it means default size in Emacs). If you want to enlarge
;;   text, set positive number. Otherwise, set negative number.
;;
;; Enjoy!

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'popwin)
(require 'face-remap)

;;; variables
(defgroup guide-key nil
  "Guide key bidings."
  :group 'help
  :prefix "guide-key/")

(defcustom guide-key/guide-key-sequence nil
  "*Key sequences to guide in `guide-key-mode'.
This variable is a list of string representation.
Both representations, like \"C-x r\" and \"\\C-xr\",
are allowed.

In addition, an element of this list can be a list with car a
symbol representing major mode, and cdr a list of key sequences
to consider only if this major mode is active."
  :type '(repeat (choice (string :tag "Prefix key sequence")
                         (cons :tag "Major-mode specific sequence"
                               (symbol :tag "Major mode")
                               (repeat (string :tag "Prefix key sequence")))))
  :group 'guide-key)

(defcustom guide-key/polling-time 0.1
  "*Polling time to check an input key sequence."
  :type 'float
  :group 'guide-key)

(defcustom guide-key/idle-delay 1.0
  "*Delay in seconds before guide buffer is displayed."
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

(defcustom guide-key/text-scale-amount 0
  "*Amount of scaling text in guide buffer.

If positive number, the text becomes larger.  If negative number,
the text becomes smaller.  Scale of the text is detemined by the
value of variable `text-scale-mode-step'."
  :type 'float
  :group 'guide-key)

(defcustom guide-key/recursive-key-sequence-flag nil
  "*If non-nil, check an input key sequence recursively.
For example, if `guide-key/guide-key-sequence' includes \"C-x\",
guide buffer is popped up when you input \"C-x r\", \"C-x 4\" and
any other prefixes following \"C-x\"."
  :type 'boolean
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

(defvar guide-key/idle-timer nil
  "Idle timer to wait before popping up guide buffer.")

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

(defun guide-key/popup-function ()
  "Popup function called after delay of `guide-key/idle-delay' second."
  (let ((key-seq (this-command-keys-vector)))
    (let ((dsc-buf (current-buffer))
	  (hi-regexp guide-key/highlight-command-regexp)
	  (max-width 0))
      (with-current-buffer (get-buffer-create guide-key/guide-buffer-name)
	(unless truncate-lines (setq truncate-lines t))   ; don't fold line
	(when indent-tabs-mode (setq indent-tabs-mode nil)) ; don't use tab as white space
	(setq mode-line-format nil)
	(text-scale-set guide-key/text-scale-amount)
	(erase-buffer)
	(describe-buffer-bindings dsc-buf key-seq)
	(when (> (guide-key/format-guide-buffer key-seq hi-regexp) 0)
	  (guide-key/close-guide-buffer)
	  (guide-key/popup-guide-buffer))))))


;;; internal functions
(defun guide-key/polling-function ()
  "Polling function executed every `guide-key/polling-time' second."
  (let ((key-seq (this-command-keys-vector)))
    (if (guide-key/popup-guide-buffer-p key-seq)
	(progn
	  (when (guide-key/update-guide-buffer-p key-seq)
	    (guide-key/turn-on-idle-timer)))
      (guide-key/close-guide-buffer))
    (setq guide-key/last-key-sequence-vector key-seq)))

(defun guide-key/popup-guide-buffer ()
  "Pop up guide buffer at `guide-key/popup-window-position'."
  (let ((last-config popwin:popup-last-config))
    (apply 'popwin:popup-buffer (get-buffer guide-key/guide-buffer-name)
           :position guide-key/popup-window-position
           :noselect t
           (cond ((popwin:position-horizontal-p guide-key/popup-window-position)
                  `(:width ,(guide-key/popup-window-size 'horizontal)))
                 ((popwin:position-vertical-p guide-key/popup-window-position)
                  `(:height ,(guide-key/popup-window-size)))))
    (setq popwin:popup-last-config last-config)))

(defun guide-key/popup-window-size (&optional horizontal)
  "Return an enough height or width of popup window to display
all key bindings in guide buffer.

If HORIZONTAL is omitted or nil, return the height of popup
window.  Otherwise, return the width of popup window"
  (with-current-buffer (get-buffer guide-key/guide-buffer-name)
    (let ((margin (if horizontal 5 1))
          (scale (expt text-scale-mode-step text-scale-mode-amount)))
      (if horizontal
          (ceiling (* scale (+ (guide-key/buffer-max-width) margin)))
        (ceiling (* scale (+ (count-lines (point-min) (point-max)) margin))))
      )))

(defun guide-key/close-guide-buffer ()
  "Close guide buffer."
  (when (eq popwin:popup-buffer (get-buffer guide-key/guide-buffer-name))
    (popwin:close-popup-window))
  (guide-key/turn-off-idle-timer)
  )

(add-hook 'pre-command-hook 'guide-key/close-guide-buffer)

(defun guide-key/update-guide-buffer-p (key-seq)
  "Return t if guide buffer should be updated."
  (not (equal guide-key/last-key-sequence-vector key-seq)))

(defun guide-key/popup-guide-buffer-p (key-seq)
  "Return t if guide buffer should be popped up."
  (and (> (length key-seq) 0)
       (or (member key-seq (mapcar 'guide-key/convert-key-sequence-to-vector
                                   (append (cl-remove-if 'listp guide-key/guide-key-sequence)
                                           (cdr (assoc major-mode guide-key/guide-key-sequence)))))
           (and guide-key/recursive-key-sequence-flag
                (guide-key/popup-guide-buffer-p (guide-key/vbutlast key-seq))))))

(defun guide-key/vbutlast (vec &optional n)
  "Return a copy of vector VEC with the last N elements removed."
  (vconcat (butlast (append vec nil) n)))

(defun guide-key/convert-key-sequence-to-vector (key-seq)
  "Convert key sequence KEY-SEQ to vector representation.
For example, both \"C-x r\" and \"\\C-xr\" are converted to [24 114]"
  (vconcat (read-kbd-macro key-seq)))

(defun guide-key/turn-on-idle-timer ()
  "Turn on an idle timer for popping up guide buffer."
  (when (null guide-key/idle-timer)
    (setq guide-key/idle-timer
          (run-with-idle-timer guide-key/idle-delay t 'guide-key/popup-function))
))

(defun guide-key/turn-off-idle-timer ()
  "Turn off the idle timer."
  (when guide-key/idle-timer
    (cancel-timer guide-key/idle-timer))
  (setq guide-key/idle-timer nil))


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
      (guide-key/insert-following-key fkey-str-list
       (popwin:position-horizontal-p guide-key/popup-window-position))
      (goto-char (point-min)))
    fkey-list-len))

(defun guide-key/insert-following-key (fkey-str-list horizontal)
  "Insert a few following keys per line.

If HORIZONTAL is omitted or nil, assume that guide buffer is
popped up at top or bottom. Otherwise, assume that guide buffer
is popped up at left or right."
  (let* ((scale (expt text-scale-mode-step text-scale-mode-amount))
         ;; Calculate the number of items per line
         (columns
          (if horizontal
              (ceiling (/ (* (length fkey-str-list) scale)
                          (- (frame-height) (if tool-bar-mode 2 0) (if menu-bar-mode 1 0))))
            (floor (/ (frame-width)
                      (* (apply 'max (mapcar 'length fkey-str-list)) scale))))))
    ;; Insert following keys by columns per line.
    (loop for fkey-str in fkey-str-list
          for column from 1
          do (insert fkey-str (if (= (mod column columns) 0) "\n" " ")))
    (align-regexp (point-min) (point-max) "\\(\\s-*\\) \\[" 1 1 t)))

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
