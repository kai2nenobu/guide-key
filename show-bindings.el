
(require 'popwin)

(defvar show-bindings:show-key-sequence '("\C-c" "\C-q" "\C-xr")
  "*Key sequences to show its bindings.")

(defvar show-bindings:polling-time 0.05
  "*Polling time to show bindings.")

(defvar show-bindings:polling-timer nil
  "Polling timer for show bindings.")

(defvar show-bindings:buffer-name "*show-bindings*"
  "Buffer name to show bindings.")

(defvar show-bindings:last-command-keys-vector nil
  "Last command keys as vector.")

;; or hook
;; (add-hook 'pre-command-hook 'show-bindings:hook-command)
;; (setq pre-command-hook nil)
;; (add-hook 'post-command-hook 'show-bindings:key-event)
;; (add-hook 'pre-command-hook 'show-this-command)

;;; functions
;;;###autoload
(define-minor-mode show-bindings-mode
  "Show bindings automatically."
  :global t
  (funcall (if show-bindings-mode
               'show-bindings:turn-on-timer
             'show-bindings:turn-off-timer)))

;;; internal functions
(defun show-bindings:popup-bindings ()
  "Pop up window to show bindings."
  (let ((cbuffer (current-buffer))
        (key-seq (this-command-keys-vector)))
    (if (show-bindings:display-popup-p key-seq)
        (when (show-bindings:update-popup-p key-seq)
          (with-current-buffer (get-buffer-create show-bindings:buffer-name)
            (erase-buffer)
            (describe-buffer-bindings cbuffer key-seq)
            (popwin:popup-buffer (current-buffer) :width 65 :position 'right :noselect t))
            ;; (popwin:popup-buffer (current-buffer) :height 20 :position 'bottom :noselect t))
          )
      (when (show-bindings:poppedup-p)
        ;; (delete-windows-on show-bindings:buffer-name))
        (popwin:close-popup-window))
      )
    (setq show-bindings:last-command-keys-vector key-seq)))

(defun show-bindings:update-popup-p (key-seq)
  "Return t if show bindings buffer should be updated."
  (not (equal show-bindings:last-command-keys-vector key-seq)))

(defun show-bindings:display-popup-p (key-seq)
  "Return t if show bindings buffer should be displayed."
  (and (> (length key-seq) 0)
       (member key-seq (mapcar 'vconcat show-bindings:show-key-sequence))
       ))

(defun show-bindings:poppedup-p ()
  "Return t if show bindings buffer is popped up."
  (eq popwin:popup-buffer (get-buffer show-bindings:buffer-name)))

(defun show-bindings:turn-on-timer ()
  "Turn on polling timer."
  (setq show-bindings:polling-timer
        (run-at-time t show-bindings:polling-time 'show-bindings:popup-bindings)))

(defun show-bindings:turn-off-timer ()
  "Turn off polling timer."
  (cancel-timer show-bindings:polling-timer))

;;; debug
(defun show-bindings:message-events ()
  ""
  (interactive)
  (message (format "lce:%S tck:%S tckv:%S lie:%S uce:%S popb:%S cls:%S"
           last-command-event
           (this-command-keys)
           (this-command-keys-vector)
           last-input-event
           unread-command-events
           popwin:popup-buffer
           (show-bindings:poppedup-p)
           )))
;; (setq ttt (run-at-time t 1 'show-bindings:message-events))
;; (cancel-timer ttt)