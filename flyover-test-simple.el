;;; simple-test.el --- Simple test for flyover-show-only-when-cursor-on-same-line

(require 'flyover)
(require 'cl-lib)

(defun simple-test ()
  "Simple test for flyover-show-only-when-cursor-on-same-line."
  (interactive)
  (with-temp-buffer
    (setq-local flyover-show-only-when-cursor-on-same-line t)
    (setq-local flyover-debug t)
    
    ;; Insert test content
    (insert "Line 1: No errors\n")
    (insert "Line 2: Has error\n")
    (insert "Line 3: No errors\n")
    
    ;; Clear the modified flag
    (set-buffer-modified-p nil)
    
    ;; Create sample errors
    (let ((errors (list
                   (list :line 2 :column 5 :level 'error :message "Test error on line 2"))))
      
      ;; Mock flyover--get-all-errors to return our test errors
      (cl-letf (((symbol-function 'flyover--get-all-errors) (lambda () errors)))
        
        ;; Test with cursor on line 1
        (goto-char (point-min))
        (message "=== CURSOR ON LINE 1 ===")
        (message "Current line: %d" (line-number-at-pos))
        (message "Buffer modified: %s" (buffer-modified-p))
        (flyover--maybe-display-errors)
        (message "Overlays: %d" (length flyover--overlays))
        
        ;; Test with cursor on line 2 (error line)
        (goto-char (point-min))
        (forward-line 1)
        (message "=== CURSOR ON LINE 2 ===")
        (message "Current line: %d" (line-number-at-pos))
        (flyover--maybe-display-errors)
        (message "Overlays: %d" (length flyover--overlays))
        
        ;; Test with cursor on line 3
        (goto-char (point-min))
        (forward-line 2)
        (message "=== CURSOR ON LINE 3 ===")
        (message "Current line: %d" (line-number-at-pos))
        (flyover--maybe-display-errors)
        (message "Overlays: %d" (length flyover--overlays))))))

(simple-test)