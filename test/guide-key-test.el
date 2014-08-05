(require 'ert)
(require 'guide-key)
(eval-when-compile
  (require 'cl))

(defconst guide-key-test/global-keybindings
  '((1 . guide-key-test/global-keybinding1)
    (2 . guide-key-test/global-keybinding2)))

(defconst guide-key-test/prefix-key (kbd "s-S-C-M-x"))

(defmacro guide-key-test/deftest (name doc-string &rest body)
  (declare (indent 1)
           (doc-string 2))
  `(ert-deftest ,(intern (concat "guide-key-test/" (symbol-name name))) ()
     ,doc-string
     ;; setup
     (loop for (event . definition) in guide-key-test/global-keybindings
           do
           (global-set-key (vconcat guide-key-test/prefix-key (vector event)) definition))
     ;; test
     ,@body
     ;; teardown
     (loop for (event . definition) in guide-key-test/global-keybindings
           do
           (global-unset-key (vconcat guide-key-test/prefix-key (vector event))))
     ))

(ert-deftest guide-key-test/get-highlight-face ()
  "Test of `guide-key/get-highlight-face'"
  (let ((guide-key/highlight-command-regexp
         '("rectangle"
           ("register" . font-lock-type-face)
           ("bookmark" . font-lock-warning-face)
           ))
        (fixtures
         '(("Prefix Command" . guide-key/prefix-command-face)
           ("string-rectangle" . guide-key/highlight-command-face)
           ("jump-to-register" . font-lock-type-face)
           ("bookmark-jump" . font-lock-warning-face)
           ("copy-rectangle-to-register" . guide-key/highlight-command-face)
           ("<NOTEXIST>" . nil)
           ))
        actual)
    (loop for (input . expected) in fixtures
          do
          (setq actual (guide-key/get-highlight-face input))
          (should (eq actual expected)))
    ))

