;;; flycheck-overlay-levels-test.el --- Test error level filtering -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the flycheck-overlay-levels customization option.
;; This tests that only specified error levels are displayed as overlays.

;;; Code:

(require 'flycheck-overlay)

(defun flycheck-overlay-test-create-mock-errors ()
  "Create mock errors of all levels for testing."
  (list
   (flycheck-error-new-at 1 0 'error "This is an error message" :checker 'test-checker)
   (flycheck-error-new-at 2 0 'warning "This is a warning message" :checker 'test-checker)
   (flycheck-error-new-at 3 0 'info "This is an info message" :checker 'test-checker)))

(defun flycheck-overlay-test-count-overlays-by-level (level)
  "Count overlays for a specific error LEVEL."
  (let ((count 0))
    (dolist (ov flycheck-overlay--overlays)
      (when (and (overlayp ov)
                 (overlay-get ov 'flycheck-error)
                 (eq (flycheck-error-level (overlay-get ov 'flycheck-error)) level))
        (setq count (1+ count))))
    count))

(defun flycheck-overlay-test-all-levels ()
  "Test that all error levels are displayed when all are enabled."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\n")
    (let ((flycheck-overlay-levels '(error warning info))
          (flycheck-overlay--overlays nil))
      (flycheck-overlay--display-errors (flycheck-overlay-test-create-mock-errors))
      (let ((error-count (flycheck-overlay-test-count-overlays-by-level 'error))
            (warning-count (flycheck-overlay-test-count-overlays-by-level 'warning))
            (info-count (flycheck-overlay-test-count-overlays-by-level 'info)))
        (and (= error-count 1)
             (= warning-count 1)
             (= info-count 1))))))

(defun flycheck-overlay-test-errors-only ()
  "Test that only errors are displayed when only errors are enabled."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\n")
    (let ((flycheck-overlay-levels '(error))
          (flycheck-overlay--overlays nil))
      (flycheck-overlay--display-errors (flycheck-overlay-test-create-mock-errors))
      (let ((error-count (flycheck-overlay-test-count-overlays-by-level 'error))
            (warning-count (flycheck-overlay-test-count-overlays-by-level 'warning))
            (info-count (flycheck-overlay-test-count-overlays-by-level 'info)))
        (and (= error-count 1)
             (= warning-count 0)
             (= info-count 0))))))

(defun flycheck-overlay-test-errors-and-warnings ()
  "Test that only errors and warnings are displayed when both are enabled."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\n")
    (let ((flycheck-overlay-levels '(error warning))
          (flycheck-overlay--overlays nil))
      (flycheck-overlay--display-errors (flycheck-overlay-test-create-mock-errors))
      (let ((error-count (flycheck-overlay-test-count-overlays-by-level 'error))
            (warning-count (flycheck-overlay-test-count-overlays-by-level 'warning))
            (info-count (flycheck-overlay-test-count-overlays-by-level 'info)))
        (and (= error-count 1)
             (= warning-count 1)
             (= info-count 0))))))

(defun flycheck-overlay-test-no-levels ()
  "Test that no overlays are displayed when no levels are enabled."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\n")
    (let ((flycheck-overlay-levels '())
          (flycheck-overlay--overlays nil))
      (flycheck-overlay--display-errors (flycheck-overlay-test-create-mock-errors))
      (let ((error-count (flycheck-overlay-test-count-overlays-by-level 'error))
            (warning-count (flycheck-overlay-test-count-overlays-by-level 'warning))
            (info-count (flycheck-overlay-test-count-overlays-by-level 'info)))
        (and (= error-count 0)
             (= warning-count 0)
             (= info-count 0))))))

(defun flycheck-overlay-run-level-tests ()
  "Run all level filtering tests and report results."
  (interactive)
  (let ((tests '(("All levels" . flycheck-overlay-test-all-levels)
                 ("Errors only" . flycheck-overlay-test-errors-only)
                 ("Errors and warnings" . flycheck-overlay-test-errors-and-warnings)
                 ("No levels" . flycheck-overlay-test-no-levels)))
        (results '()))
    (dolist (test tests)
      (let* ((name (car test))
             (func (cdr test))
             (result (condition-case err
                         (if (funcall func) "PASS" "FAIL")
                       (error (format "ERROR: %s" err)))))
        (push (cons name result) results)))
    
    (with-current-buffer (get-buffer-create "*Flycheck Overlay Level Tests*")
      (erase-buffer)
      (insert "Flycheck Overlay Level Filtering Test Results:\n")
      (insert "===============================================\n\n")
      (dolist (result (reverse results))
        (insert (format "%-25s: %s\n" (car result) (cdr result))))
      (insert "\nTest completed.\n")
      (display-buffer (current-buffer)))))

(defun flycheck-overlay-flymake-test-buffer ()
  "Create a test buffer with flymake diagnostics to verify conversion."
  (interactive)
  (let ((buffer (get-buffer-create "*Flycheck Overlay Flymake Test*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "// This is a test buffer for flymake diagnostics\n")
      (insert "function testFunction() {\n")
      (insert "    var unused_variable = 42;  // This should trigger a warning\n")
      (insert "    console.log(undefined_var); // This should trigger an error\n")
      (insert "    // TODO: This might trigger an info diagnostic\n")
      (insert "}\n")
      (insert "\n")
      (insert "// Test different error levels:\n")
      (insert "syntax error here;  // Error\n")
      (insert "deprecated_function(); // Warning\n")
      (insert "// Note: This is just informational // Info\n")
      
      ;; Set up the buffer for testing
      (js-mode)
      (flycheck-overlay-mode 1)   
      
      ;; Create mock flymake diagnostics
      (let ((diagnostics (list
                          ;; Error diagnostic
                          (flymake-make-diagnostic buffer 4 20 'flymake-error "undefined_var is not defined")
                          ;; Warning diagnostic  
                          (flymake-make-diagnostic buffer 3 8 'flymake-warning "unused_variable is assigned but never used")
                          ;; Info diagnostic
                          (flymake-make-diagnostic buffer 5 4 'flymake-note "Consider using const instead of var")
                          ;; Another error
                          (flymake-make-diagnostic buffer 9 0 'flymake-error "Unexpected token 'error'")
                          ;; Another warning
                          (flymake-make-diagnostic buffer 10 0 'flymake-warning "deprecated_function is deprecated"))))
        
        ;; Mock flymake-diagnostics function to return our test diagnostics
        (setq-local flymake-diagnostics-function (lambda () diagnostics))
        
        ;; Enable flymake mode and set up overlay checkers
        (flymake-mode 1)
        (setq-local flycheck-overlay-checkers '(flymake))
        
        ;; Force display of errors
        (flycheck-overlay--display-errors)
        
        (goto-char (point-min))
        (message "Flymake test buffer created with %d diagnostics. Use M-x flycheck-overlay-toggle to toggle overlays."
                 (length diagnostics))))
    
    (switch-to-buffer buffer)))

(defun flycheck-overlay-flymake-test-levels ()
  "Test flymake level filtering interactively."
  (interactive)
  (let ((buffer (get-buffer-create "*Flycheck Overlay Flymake Level Test*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert ";; Flymake Level Filtering Test\n")
      (insert ";; Use the following commands to test different level configurations:\n")
      (insert ";;\n")
      (insert ";; Show all levels:\n")
      (insert ";; (setq flycheck-overlay-levels '(error warning info))\n")
      (insert ";;\n") 
      (insert ";; Show only errors:\n")
      (insert ";; (setq flycheck-overlay-levels '(error))\n")
      (insert ";;\n")
      (insert ";; Show errors and warnings:\n")
      (insert ";; (setq flycheck-overlay-levels '(error warning))\n")
      (insert ";;\n")
      (insert ";; Show no levels:\n")
      (insert ";; (setq flycheck-overlay-levels '())\n")
      (insert "\n")
      (insert "function example() {\n")
      (insert "    let unused = 'warning level';\n")
      (insert "    console.log(undefined_var); // error level\n") 
      (insert "    // TODO: info level diagnostic\n")
      (insert "}\n")
      
      (js-mode)
      (flycheck-overlay-mode 1)
      
      ;; Create diagnostics for each level
      (let ((diagnostics (list
                          (flymake-make-diagnostic buffer 3 16 'flymake-error "undefined_var is not defined")
                          (flymake-make-diagnostic buffer 2 8 'flymake-warning "unused is assigned but never used") 
                          (flymake-make-diagnostic buffer 4 4 'flymake-note "Consider adding more specific TODO details"))))
        
        (setq-local flymake-diagnostics-function (lambda () diagnostics))
        (flymake-mode 1)
        (setq-local flycheck-overlay-checkers '(flymake))
        
        ;; Start with all levels enabled
        (setq-local flycheck-overlay-levels '(error warning info))
        (flycheck-overlay--display-errors)
        
        (goto-char (point-min))
        (message "Flymake level test buffer created. Evaluate the setq expressions above to test different level configurations.")))
    
    (switch-to-buffer buffer)))

;; (switch-to-buffer (get-buffer-create "*Flycheck Overlay Level Tests*"))
;; (flycheck-overlay-run-level-tests)

;; (switch-to-buffer (flycheck-overlay-test-buffer))
;; (switch-to-buffer (flycheck-overlay-test-no-levels))
;; (switch-to-buffer (flycheck-overlay-flymake-test-buffer))
;; (switch-to-buffer (flycheck-overlay-flymake-test-levels))

(provide 'flycheck-overlay-levels-test)
;;; flycheck-overlay-levels-test.el ends here
