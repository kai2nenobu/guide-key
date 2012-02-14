
(require 'popwin)

(defvar guide-key:show-key-sequence '("C-c" "C-c &" "C-q" "C-x r" "C-x 4" "C-4")
  "*Key sequences to show its bindings.")

(defvar guide-key:polling-time 0.05
  "*Polling time to show bindings.")

(defvar guide-key:polling-timer nil
  "Polling timer for show bindings.")

(defvar guide-key:buffer-name "*guide-key*"
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
  (funcall (if guide-key-mode
               'guide-key:turn-on-timer
             'guide-key:turn-off-timer)))

;;; internal functions
(defun guide-key:popup-bindings ()
  "Pop up window to show bindings."
  (let ((cbuffer (current-buffer))
        (key-seq (this-command-keys-vector)))
    (if (guide-key:display-popup-p key-seq)
        (when (guide-key:update-popup-p key-seq)
          (with-current-buffer (get-buffer-create guide-key:buffer-name)
            (erase-buffer)
            (describe-buffer-bindings cbuffer key-seq)
            (guide-key:format-guide-buffer key-seq)
            (popwin:popup-buffer (current-buffer) :width 65 :position 'right :noselect t))
            ;; (popwin:popup-buffer (current-buffer) :height 20 :position 'bottom :noselect t))
          )
      (when (guide-key:poppedup-p)
        ;; (delete-windows-on guide-key:buffer-name))
        (popwin:close-popup-window t))
      )
    (setq guide-key:last-command-keys-vector key-seq)))

(defun guide-key:update-popup-p (key-seq)
  "Return t if show bindings buffer should be updated."
  (not (equal guide-key:last-command-keys-vector key-seq)))

(defun guide-key:display-popup-p (key-seq)
  "Return t if show bindings buffer should be displayed."
  (and (> (length key-seq) 0)
       (member key-seq (mapcar 'guide-key:convert-key-sequence-to-vector
                               guide-key:show-key-sequence))
       ))

(defun guide-key:convert-key-sequence-to-vector (key-seq)
  "Convert key sequence KEY-SEQ to vector representation."
  (vconcat (read-kbd-macro key-seq)))

(defun guide-key:poppedup-p ()
  "Return t if show bindings buffer is popped up."
  (eq popwin:popup-buffer (get-buffer guide-key:buffer-name)))

(defun guide-key:turn-on-timer ()
  "Turn on polling timer."
  (setq guide-key:polling-timer
        (run-at-time t guide-key:polling-time 'guide-key:popup-bindings)))

(defun guide-key:turn-off-timer ()
  "Turn off polling timer."
  (cancel-timer guide-key:polling-timer))

(defun guide-key:format-guide-buffer (key-seq)
  "Format a guide buffer."
  (let ((key-dsc (key-description key-seq))
        (last-end-pt 1))
    (untabify (point-min) (point-max))    ; replace tab to space
    (beginning-of-buffer)
    (while (re-search-forward
            (format "^%s \\([^ \t]+\\)\\([ \t]+\\)\\(\\(?:[^ \t\n]+ ?\\)+\\)$" key-dsc) nil t)
      (replace-match "[\\1]\\2\\3")
      (when (> (- (point-at-bol) last-end-pt) 0)
        (delete-region last-end-pt (point-at-bol)))
      (setq last-end-pt (point-at-bol 2)))
    (delete-region last-end-pt (progn (end-of-buffer) (point)))
    (beginning-of-buffer)
    ))

;;; debug
(defun guide-key:message-events ()
  ""
  (interactive)
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

(guide-key-mode t)
