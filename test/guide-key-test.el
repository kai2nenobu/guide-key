(require 'ert)
(require 'guide-key)
(eval-when-compile
  (require 'cl))

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

