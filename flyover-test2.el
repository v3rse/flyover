;;; flycheck-overlay-test.el --- Test utilities for flycheck-overlay -*- lexical-binding: t -*-

(require 'flycheck-overlay)
(require 'ert)

(defun flycheck-overlay-test-insert-errors ()
  "Insert sample errors into current buffer for testing.
Returns a list of created errors for verification."
  (let ((errors '()))
    ;; Clear buffer
    (erase-buffer)
    
    ;; Insert some code with errors
    (insert "           private val _navigationEvent = MutableSharedFlow<FaqNavigationEvent>()
\n")

    ;; Create error objects
    (push (flycheck-error-new-at 0 1 'error "Error at line 0 column 1") errors)

    ;; Display the errors
    (flycheck-overlay--display-errors errors)
    
    errors))

(defun flycheck-overlay-test-buffer ()
  "Create a new buffer with test errors and return it."
  (let ((test-buffer (get-buffer-create "*flycheck-overlay-test*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (flycheck-overlay-mode 1)
      (flycheck-overlay-test-insert-errors))
    test-buffer))

;; Example usage:
(switch-to-buffer (flycheck-overlay-test-buffer))

(provide 'flycheck-overlay-test)
;;; flycheck-overlay-test.el ends here
