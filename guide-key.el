
;;; TODO:
;; - enrichment of guide buffer
;; - automatically guide all following keys
;; - pop up guide buffer at top or bottom
;; - cooperate with polling of popwin.el?

(require 'popwin)

(defvar guide-key:show-key-sequence '("C-c" "C-c &" "C-q" "C-x r" "C-S-r" "C-x 4" "C-4")
  "*Key sequences to show its bindings.")

(defvar guide-key:polling-time 0.01
  "*Polling time to show bindings.")

(defface guide-key:key-face
  '((t (:foreground "red")))
  "Face for key"
  :group 'guide-key)

(defface guide-key:prefix-command-face
  '((t (:foreground "cyan")))
  "Face for prefix command"
  :group 'guide-key)

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
        (key-seq (this-command-keys-vector))
        (max-width 0))
    (if (guide-key:display-popup-p key-seq)
        (when (guide-key:update-popup-p key-seq)
          (with-current-buffer (get-buffer-create guide-key:buffer-name)
            (erase-buffer)
            (describe-buffer-bindings cbuffer key-seq)
            (guide-key:format-guide-buffer key-seq)
            (if (> (setq max-width (guide-key:buffer-max-width)) 0)
                (popwin:popup-buffer (current-buffer)
                                     :width (+ max-width 3) :position 'right :noselect t)
              (message "No following key.")))))
    (setq guide-key:last-command-keys-vector key-seq)))

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
    (goto-char (point-min))
    ;; delete extra strings and format key guide
    (while (re-search-forward
            (format "^%s \\([^ \t]+\\)\\([ \t]+\\)\\(\\(?:[^ \t\n]+ ?\\)+\\)$" key-dsc) nil t)
      (replace-match "[\\1]\\2\\3")
      (when (> (- (point-at-bol) last-end-pt) 0)
        (delete-region last-end-pt (point-at-bol)))
      (setq last-end-pt (point-at-bol 2)))
    (delete-region last-end-pt (point-max))
    ;; fontify key guide
    (goto-char (point-min))
    (while (re-search-forward
            "\\(\\[[^ \t]+\\]\\)\\([ \t]+\\)\\(\\(?:[^ \t\n]+ ?\\)+\\)$" nil t)
      (put-text-property (match-beginning 1) (1+ (match-beginning 1)) 'face 'guide-key:key-face)
      (put-text-property (1- (match-end 1)) (match-end 1) 'face 'guide-key:key-face)
      )
    (goto-char (point-min))
    ))

(defun guide-key:buffer-max-width ()
  "Return max width in current buffer."
  (let ((buf-str (buffer-substring-no-properties (point-min) (point-max))))
    (apply 'max (mapcar 'length (split-string buf-str "\n")))))

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
