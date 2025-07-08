;;; debug-show-only.el --- Debug the flyover-show-only-when-cursor-on-same-line fix

(require 'flyover)

(defun debug-show-only-fix ()
  "Debug the fix for flyover-show-only-when-cursor-on-same-line."
  (interactive)
  (let ((test-buffer (get-buffer-create "*flyover-debug-test*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (flyover-mode 1)
      (setq-local flyover-show-only-when-cursor-on-same-line t)
      (setq-local flyover-debug t)
      
      ;; Insert test content
      (insert "Line 1: No errors\n")
      (insert "Line 2: Has error\n")
      (insert "Line 3: No errors\n")
      
      ;; Create sample errors using our internal structure
      (let ((errors (list
                     (list :line 2 :column 5 :level 'error :message "Test error on line 2"))))
        
        ;; Position cursor at line 1
        (goto-char (point-min))
        (message "DEBUG: Current line: %d, Error line: %d" (line-number-at-pos) 2)
        (message "DEBUG: All errors: %S" errors)
        
        ;; Test filtering
        (let ((filtered-errors (seq-filter 
                                (lambda (err)
                                  (let ((err-line (plist-get err :line)))
                                    (message "DEBUG: Checking error line %d against current line %d" err-line (line-number-at-pos))
                                    (= err-line (line-number-at-pos))))
                                errors)))
          (message "DEBUG: Filtered errors for line 1: %S" filtered-errors))
        
        ;; Position cursor at line 2 (error line)
        (goto-char (point-min))
        (forward-line 1)
        (message "DEBUG: Current line: %d, Error line: %d" (line-number-at-pos) 2)
        
        ;; Test filtering
        (let ((filtered-errors (seq-filter 
                                (lambda (err)
                                  (let ((err-line (plist-get err :line)))
                                    (message "DEBUG: Checking error line %d against current line %d" err-line (line-number-at-pos))
                                    (= err-line (line-number-at-pos))))
                                errors)))
          (message "DEBUG: Filtered errors for line 2: %S" filtered-errors)
          
          ;; Try to display the filtered errors
          (flyover--display-errors filtered-errors)
          (message "DEBUG: Overlays after display: %d" (length flyover--overlays)))))
    
    (switch-to-buffer test-buffer)))

(debug-show-only-fix)