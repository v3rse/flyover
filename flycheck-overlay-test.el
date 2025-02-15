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
    (insert "Line 0: This line has an error\n")
    (insert "Line 1: This line has an error\n")
    (insert "Line 2: This line has a warning\n")
    (insert "Line 3: This line has an info message\n")
    (insert "Line 4: Multiple errors on one line\n")
    
    ;; Create error objects
    (push (flycheck-error-new-at 0 6 'error "Sample error message") errors)
    (push (flycheck-error-new-at 1 6 'error "Sample error message") errors)
    (push (flycheck-error-new-at 2 6 'warning "Sample warning message") errors)
    (push (flycheck-error-new-at 3 6 'info "Sample info message") errors)
    (push (flycheck-error-new-at 4 6 'error "First error on line") errors)
    (push (flycheck-error-new-at 4 20 'warning "Second error on line") errors)

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
