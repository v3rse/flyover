;;; flycheck-overlay-test.el --- Test utilities for flycheck-overlay -*- lexical-binding: t -*-

;; Only load if flycheck-overlay is available
(condition-case nil
    (require 'flycheck-overlay)
  (error
   (message "Warning: flycheck-overlay not available. Some tests may not work.")))

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
    (insert "Line 5:\n")
    (insert "Line 6:\n")
    (insert "Line 7: This line has no errors or warnings \n")
    (insert "Line 8:\n")

    ;; Create error objects
    (push (flycheck-error-new-at 0 1 'error "Error at line 0 column 1") errors)
    (push (flycheck-error-new-at 2 2 'error "Error at line 2 column 2") errors)
    (push (flycheck-error-new-at 3 3 'warning "Warning at line 3 column 3") errors)
    (push (flycheck-error-new-at 4 4 'info "Info at line 4 and column 4") errors)
    (push (flycheck-error-new-at 5 5 'error "Error at line 5 column 5") errors)
    (push (flycheck-error-new-at 5 20 'warning "Warning at line 5 column 20") errors)
    (push (flycheck-error-new-at 9 5 'warning "Extra blank line detected at line 8 and column 5") errors)

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

;; Tests for message wrapping functionality

(ert-deftest flycheck-overlay-test-wrap-message-short ()
  "Test that short messages are not wrapped."
  (let ((flycheck-overlay-wrap-messages t)
        (flycheck-overlay-max-line-length 80))
    (should (equal (flycheck-overlay--wrap-message "Short message" 80)
                   '("Short message")))))

(ert-deftest flycheck-overlay-test-wrap-message-long ()
  "Test that long messages are wrapped correctly."
  (let ((flycheck-overlay-wrap-messages t)
        (flycheck-overlay-max-line-length 20))
    (should (equal (flycheck-overlay--wrap-message "This is a very long error message that should be wrapped" 20)
                   '("This is a very long" "error message that" "should be wrapped")))))

(ert-deftest flycheck-overlay-test-wrap-message-disabled ()
  "Test that wrapping is disabled when flycheck-overlay-wrap-messages is nil."
  (let ((flycheck-overlay-wrap-messages nil)
        (flycheck-overlay-max-line-length 20))
    (should (equal (flycheck-overlay--wrap-message "This is a very long error message that should not be wrapped" 20)
                   '("This is a very long error message that should not be wrapped")))))

(ert-deftest flycheck-overlay-test-wrap-message-single-word ()
  "Test wrapping with a single long word."
  (let ((flycheck-overlay-wrap-messages t)
        (flycheck-overlay-max-line-length 10))
    (should (equal (flycheck-overlay--wrap-message "supercalifragilisticexpialidocious" 10)
                   '("supercalifragilisticexpialidocious")))))

(ert-deftest flycheck-overlay-test-wrap-message-empty ()
  "Test wrapping with empty message."
  (let ((flycheck-overlay-wrap-messages t)
        (flycheck-overlay-max-line-length 80))
    (should (equal (flycheck-overlay--wrap-message "" 80)
                   '()))))

(ert-deftest flycheck-overlay-test-wrap-message-exact-length ()
  "Test wrapping with message exactly at max length."
  (let ((flycheck-overlay-wrap-messages t)
        (flycheck-overlay-max-line-length 20))
    (should (equal (flycheck-overlay--wrap-message "Exactly twenty chars" 20)
                   '("Exactly twenty chars")))))

(ert-deftest flycheck-overlay-test-wrap-message-multiple-spaces ()
  "Test wrapping with multiple spaces."
  (let ((flycheck-overlay-wrap-messages t)
        (flycheck-overlay-max-line-length 20))
    (should (equal (flycheck-overlay--wrap-message "Word    with    multiple    spaces    should    wrap" 20)
                   '("Word with multiple" "spaces should wrap")))))

(defun flycheck-overlay-test-multiline-errors ()
  "Insert sample errors with long messages for multiline testing.
Returns a list of created errors for verification."
  (let ((errors '()))
    ;; Clear buffer
    (erase-buffer)
    
    ;; Insert some code with errors
    (insert "function calculateComplexValue(param) {\n")
    (insert "    return param * undefined_variable;\n")
    (insert "}\n")
    (insert "\n")
    (insert "const result = calculateComplexValue(42);\n")

    ;; Create error objects with long messages
    (push (flycheck-error-new-at 2 20 'error 
           "ReferenceError: undefined_variable is not defined. This variable has not been declared in the current scope. Please check if you meant to use a different variable name or declare this variable before using it.")
          errors)
    (push (flycheck-error-new-at 5 7 'warning
           "Variable 'result' is assigned a value but never used. Consider removing this variable or using it somewhere in your code to avoid this warning.")
          errors)
    (push (flycheck-error-new-at 1 1 'info
           "Consider adding JSDoc documentation for this function. This will help other developers understand the purpose, parameters, and return value of this function.")
          errors)

    ;; Display the errors
    (flycheck-overlay--display-errors errors)
    
    errors))

(defun flycheck-overlay-test-multiline-buffer ()
  "Create a new buffer with multiline test errors and return it."
  (let ((test-buffer (get-buffer-create "*flycheck-overlay-multiline-test*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (flycheck-overlay-mode 1)
      ;; Enable wrapping for testing
      (setq-local flycheck-overlay-wrap-messages t)
      (setq-local flycheck-overlay-max-line-length 50)
      (flycheck-overlay-test-multiline-errors))
    test-buffer))

(ert-deftest flycheck-overlay-test-multiline-display ()
  "Test that multiline overlays are created properly."
  (with-temp-buffer
    (let ((flycheck-overlay-wrap-messages t)
          (flycheck-overlay-max-line-length 30))
      ;; Setup buffer content
      (insert "some code here\n")
      (insert "more code\n")
      
      ;; Create a long error message
      (let* ((long-msg "This is a very long error message that should definitely be wrapped across multiple lines")
             (error (flycheck-error-new-at 2 5 'error long-msg))
             (region (cons (line-beginning-position 2) (line-end-position 2))))
        
        ;; Test that the message is wrapped
        (let ((wrapped (flycheck-overlay--wrap-message long-msg 30)))
          (should (> (length wrapped) 1))
          (should (cl-every (lambda (line) (<= (length line) 30)) wrapped)))))))

(ert-deftest flycheck-overlay-test-line-position-offset ()
  "Test that line position offset works correctly."
  (with-temp-buffer
    (let ((flycheck-overlay-line-position-offset 2))
      ;; Setup buffer content
      (insert "line 1\n")
      (insert "line 2 with error\n")
      (insert "line 3\n")
      (insert "line 4\n")
      
      ;; Create error at line 2
      (let* ((error (flycheck-error-new-at 2 5 'error "Test error"))
             (region (progn
                       (goto-char (point-min))
                       (forward-line 1)
                       (cons (line-beginning-position) (line-end-position))))
             (overlay (flycheck-overlay--create-overlay region 'error "Test error" error)))
        
        (should (overlayp overlay))
        ;; The overlay should start at line 2 but extend to line 4 (2 + offset)
        (goto-char (overlay-start overlay))
        (should (= (line-number-at-pos) 2))))))

;; Test runner function
(defun flycheck-overlay-run-tests ()
  "Run all flycheck-overlay tests."
  (interactive)
  (ert-run-tests-interactively "flycheck-overlay-test-"))

;; Example usage:
;; (switch-to-buffer (flycheck-overlay-test-buffer))
(switch-to-buffer (flycheck-overlay-test-multiline-buffer))

(provide 'flycheck-overlay-test)
;;; flycheck-overlay-test.el ends here
