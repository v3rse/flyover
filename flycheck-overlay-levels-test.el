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

(provide 'flycheck-overlay-levels-test)
;;; flycheck-overlay-levels-test.el ends here
