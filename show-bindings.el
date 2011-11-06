
(require 'popwin)

(defvar show-bindings-buffer-name "*show-bindings*"
  "Buffer name to show bindings.")



(with-current-buffer (get-buffer-create "*show-bindings*")
  (describe-buffer-bindings (current-buffer) (kbd "C-x 4"))
  (display-buffer (current-buffer)))
