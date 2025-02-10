;;; flycheck-overlay.el --- Display Flycheck errors with overlays -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Mikael Konradsson <mikael.konradsson@outlook.com>
;; Version: 0.3.9
;; Package-Requires: ((emacs "25.1") (flycheck "0.23") (cl-lib "0.5"))
;; Keywords: convenience, tools
;; URL: https://github.com/yourusername/flycheck-overlay

;;; Commentary:
;; This package provides a way to display Flycheck errors using overlays.
;; It offers customization options for icons, colors, and display behavior.

;;; Code:

;; (define-advice flycheck-overlays-in (:override (_ _) disable-sorting)
;;   "Temporarily disable overlay sorting to debug issues."
;;   nil)

(require 'flycheck)
(require 'cl-lib)

(defgroup flycheck-overlay nil
  "Display Flycheck errors using overlays."
  :prefix "flycheck-overlay-"
  :group 'flycheck)

(defvar-local flycheck-overlay--overlays nil
  "List of overlays used in the current buffer.")

(defvar flycheck-overlay--debounce-timer nil
  "Timer used for debouncing error checks.")

(defvar flycheck-overlay-regex-mark-quotes "\\('[^']+'\\|\"[^\"]+\"\\|\{[^\}]+\}\\)"
  "Regex used to mark both single and double quoted text.")

(defvar flycheck-overlay-regex-mark-parens "\\(\([^\)]+\)\\)"
  "Regex used to mark parentheses.")

(defvar flycheck-overlay-checker-regex "^[^\"'(]*?:\\(.*\\)"
  "Regex used to match the checker name at the start of the error message.")

(defface flycheck-overlay-error
  '((t :background "#453246"
       :foreground "#ea8faa"
       :height 0.9
       :weight normal))
  "Face used for error overlays."
  :group 'flycheck-overlay)

(defface flycheck-overlay-warning
  '((t :background "#331100"
       :foreground "#DCA561"
       :height 0.9
       :weight normal))
  "Face used for warning overlays."
  :group 'flycheck-overlay)

(defface flycheck-overlay-info
  '((t :background "#374243"
       :foreground "#a8e3a9"
       :height 0.9
       :weight normal))
  "Face used for info overlays."
  :group 'flycheck-overlay)

(defface flycheck-overlay-marker
  '((t :inherit font-lock-number-face
       :height 0.9
       :weight bold))
  "Face used for info overlays."
  :group 'flycheck-overlay)

(defcustom flycheck-overlay-info-icon " "
  "Icon used for information."
  :type 'string
  :group 'flycheck-overlay)

(defcustom flycheck-overlay-warning-icon " "
  "Icon used for warnings."
  :type 'string
  :group 'flycheck-overlay)

(defcustom flycheck-overlay-error-icon " "
  "Icon used for warnings."
  :type 'string
  :group 'flycheck-overlay)

(defcustom flycheck-overlay-virtual-line-icon nil
  "Icon used for the virtual line."
  :type 'string
  :group 'flycheck-overlay)

(defcustom flycheck-overlay-percent-darker 50
  "Icon background percent darker.
Based on foreground color"
  :type 'integer
  :group 'flycheck-overlay)

(defcustom flycheck-overlay-show-at-eol nil
  "Show error messages at the end of the line."
  :type 'boolean
  :group 'flycheck-overlay)

(defcustom flycheck-overlay-hide-when-cursor-is-on-same-line nil
  "Hide error messages when the cursor is on the same line."
  :type 'boolean
  :group 'flycheck-overlay)

(defcustom flycheck-overlay-hide-checker-name t
  "Hide the checker name in the error message."
  :type 'boolean
  :group 'flycheck-overlay)

(defcustom flycheck-overlay-show-virtual-line t
  "Show a virtual line to highlight the error a bit more"
  :type 'boolean
  :group 'flycheck-overlay)

(defcustom flycheck-overlay-icon-left-padding 0.9
  "Padding to the left of the icon."
  :type 'number
  :group 'flycheck-overlay)

(defcustom flycheck-overlay-icon-right-padding 0.5
  "Padding to the right of the icon."
  :type 'number
  :group 'flycheck-overlay)

(defcustom flycheck-overlay-debug nil
  "Enable debug messages for flycheck-overlay."
 :type 'boolean
 :group 'flycheck-overlay)

(defcustom flycheck-overlay-debounce-interval 0.2
  "Time in seconds to wait before checking and displaying errors after a change."
  :type 'number
  :group 'flycheck-overlay)

(defcustom flycheck-overlay-virtual-line-type 'curved-dotted-arrow
  "Arrow used to point to the error.
Provides various line styles including straight, curved, bold, and dotted variations,
with and without arrow terminators."
  :type '(choice
          ;; Basic styles (no arrow)
          (const :tag "No indicator" nil)
          (const :tag "Straight line" line-no-arrow)
          (const :tag "Curved line" curved-line-no-arrow)
          (const :tag "Double line" double-line-no-arrow)
          (const :tag "Bold line" bold-line-no-arrow)
          (const :tag "Dotted line" dotted-line-no-arrow)
          
          ;; Straight variants with arrow
          (const :tag "Straight line + arrow" straight-arrow)
          (const :tag "Double line + arrow" double-line-arrow)
          (const :tag "Bold line + arrow" bold-arrow)
          (const :tag "Dotted line + arrow" dotted-arrow)
          
          ;; Curved variants with arrow
          (const :tag "Curved line + arrow" curved-arrow)
          (const :tag "Curved bold + arrow" curved-bold-arrow)
          (const :tag "Curved double + arrow" curved-double-arrow)
          (const :tag "Curved dotted + arrow" curved-dotted-arrow))
  :group 'flycheck-overlay)

(defun flycheck-overlay-get-arrow-type ()
  "Return the arrow character based on the selected style."
  (pcase flycheck-overlay-virtual-line-type
    ;; Basic styles (no arrow)
    ('line-no-arrow "└──")
    ('curved-line-no-arrow "╰──")
    ('double-line-no-arrow "╚══")
    ('bold-line-no-arrow "┗━━")
    ('dotted-line-no-arrow "└┈┈")
    
    ;; Straight variants with arrow
    ('straight-arrow "└──►")
    ('double-line-arrow "╚══►")
    ('bold-arrow "┗━━►")
    ('dotted-arrow "└┈┈►")
    
    ;; Curved variants with arrow
    ('curved-arrow "╰──►")
    ('curved-bold-arrow "╰━━►")
    ('curved-double-arrow "╰══►")
    ('curved-dotted-arrow "╰┈┈►")
    
    ;; Default/fallback
    ('no-arrow "")
    (_ "→")))

(defun flycheck-overlay-get-arrow ()
  "Return the arrow character based on the selected style."
  (if (not flycheck-overlay-show-virtual-line)
      ""
    (if (not flycheck-overlay-virtual-line-icon)
        (flycheck-overlay-get-arrow-type)
      flycheck-overlay-virtual-line-icon)))

(defun flycheck-overlay--sort-errors (errors)
  "Safely sort ERRORS by their buffer positions.
This function filters out invalid errors and sorts the remaining ones."
  (condition-case nil
      (let ((sorted-errors
             (seq-filter
              (lambda (err)
                (and (flycheck-error-p err)
                     (let ((line (flycheck-error-line err))
                           (column (flycheck-error-column err)))
                       (and (numberp line) (>= line 0)
                            (or (not column) (and (numberp column) (>= column 0)))))))
              errors)))
        (when flycheck-overlay-debug
          (message "Debug: Sorted errors: %S" sorted-errors))
        sorted-errors)
    (error errors)))

(defun flycheck-overlay--get-safe-position (line column)
  "Get a safe buffer position for LINE and COLUMN.
LINE and COLUMN are 1-based positions in the buffer.
Returns a buffer position that is guaranteed to be within bounds.
When COLUMN is 0 or nil, finds first non-whitespace character on the line."
  (when (or (null line) (not (numberp line)))
    (setq line 1))
  (when (or (null column) (not (numberp column)))
    (setq column 0))
  (save-restriction
    (widen)
    (save-excursion
      (condition-case err
          (progn
            (goto-char (point-min))
            (when (>= line 0)
              (forward-line (1- line)))
            (if (<= column 0)
                (progn
                  (beginning-of-line)
                  (skip-chars-forward " \t"))
              (forward-char (min (1- column)
                               (- (line-end-position) (point)))))
            (point))
        (error
         (when flycheck-overlay-debug
           (message "Debug: Error in get-safe-position: %S for line %S col %S" 
                    err line column))
         (point-min))))))


(defun flycheck-overlay--get-error-region (err)
  "Get the start and end position for ERR.
ERR is a Flycheck error object.
Returns a cons cell (START . END) representing the region."
  (condition-case region-err
      (progn
        (unless (flycheck-error-p err)
          (signal 'wrong-type-argument `(flycheck-error-p ,err)))
        (let* ((line (flycheck-error-line err))
               (column (flycheck-error-column err))
               (start-pos (flycheck-overlay--get-safe-position line column)))
          (when flycheck-overlay-debug
            (message "Debug: line=%S column=%S start-pos=%S" line column start-pos))
          (when start-pos
            (save-excursion
              (goto-char start-pos)
              (let ((end-pos (line-end-position)))
                (when flycheck-overlay-debug
                  (message "Debug: end-pos=%S" end-pos))
                (when (and (integer-or-marker-p end-pos)
                           (<= end-pos (point-max)))
                  (cons start-pos end-pos)))))))
    (error
     (when flycheck-overlay-debug
       (message "Debug region: Error getting region: %S" region-err))
     nil)))

(defun flycheck-overlay--create-overlay (region type msg)
  "Create an overlay at REGION of TYPE with message MSG.
REGION should be a cons cell (BEG . END) of buffer positions."
  (condition-case ov-err
      (when (and region (consp region)
                 (integer-or-marker-p (car region))
                 (integer-or-marker-p (cdr region)))
        (let* ((beg (max (point-min) (car region)))
               (end (min (point-max) (cdr region)))
               (next-line-beg (save-excursion
                                (if flycheck-overlay-show-at-eol
                                    end
                                    (progn
                                      (goto-char end)
                                      (line-beginning-position 2)))))
               (face (flycheck-overlay--get-face type))
               (overlay (make-overlay beg next-line-beg nil t nil)))
          (flycheck-overlay--configure-overlay overlay face msg beg)
          overlay))
    (error
     (message "Error creating overlay: %S" ov-err)
     nil)))

(defun flycheck-overlay--get-face (type)
  "Return the face corresponding to the error TYPE."
  (pcase type
    ('error 'flycheck-overlay-error)
    ('warning 'flycheck-overlay-warning)
    ('info 'flycheck-overlay-info)
    (_ 'flycheck-overlay-warning)))

(defun flycheck-overlay--get-indicator (type)
  "Return the indicator string corresponding to the error TYPE."
  (let* ((props (pcase type
                  ('flycheck-overlay-error
                   (cons flycheck-overlay-error-icon 'flycheck-overlay-error))
                  ('flycheck-overlay-warning
                   (cons flycheck-overlay-warning-icon 'flycheck-overlay-warning))
                  ('flycheck-overlay-info
                   (cons flycheck-overlay-info-icon 'flycheck-overlay-info))
                  (_
                   (cons flycheck-overlay-info-icon 'flycheck-overlay-info))))
         (icon (car props))
         (face-name (cdr props))
         (height (face-attribute face-name :height))
         (color (face-attribute face-name :foreground))
         (bg-color (flycheck-overlay--darken-color color flycheck-overlay-percent-darker)))
    
    (concat
     ;; Left padding
     (propertize " "
                 'face `(:background ,bg-color, :height ,height)
                 'display '(space :width flycheck-overlay-icon-left-padding))
     ;; Icon
     (propertize icon
                 'face `(:foreground ,color :background ,bg-color, :height ,height)
                 'display '(raise -0.02))
     ;; Right padding
     (propertize " "
                 'face `(:background ,bg-color, :height ,height)
                 'display '(space :width flycheck-overlay-icon-right-padding)))))

(defun flycheck-overlay--configure-overlay (overlay face msg beg)
  "Configure the OVERLAY with FACE and MSG starting at BEG."
  (overlay-put overlay 'flycheck-overlay t)
  (let* ((col-pos (save-excursion
                    (goto-char beg)
                    (current-column)))
         (existing-bg (face-background face nil t))
         (indicator (flycheck-overlay--get-indicator face))
         (display-msg (concat " " msg " "))
         (virtual-line (if flycheck-overlay-show-virtual-line
                           (propertize (flycheck-overlay-get-arrow)
                            'face `(:foreground ,(face-foreground face nil t)))))
         (display-string (propertize display-msg
                                     'face face
                                     'cursor-sensor-functions nil
                                     'cursor-intangible t
                                     'rear-nonsticky t))  ; Remove cursor-intangible
         (marked-string (flycheck-overlay--mark-all-symbols
                         :input display-string
                         :regex flycheck-overlay-regex-mark-quotes
                         :property `(:inherit flycheck-overlay-marker :background ,existing-bg)))
         (overlay-string (flycheck-overlay--create-overlay-string col-pos virtual-line indicator marked-string existing-bg)))
    (if (and flycheck-overlay-show-at-eol
             (< (+ col-pos (length display-msg)) (window-width)))
        (overlay-put overlay 'after-string (propertize overlay-string
                                                       'rear-nonsticky t
                                                       'cursor-sensor-functions nil
                                                       'cursor-intangible t
                                                       'field nil))
      (overlay-put overlay 'after-string (propertize (concat overlay-string "\n")
                                                     'rear-nonsticky t
                                                     'cursor-sensor-functions nil
                                                     'cursor-intangible t
                                                     'field nil)))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'priority 1000)
    (overlay-put overlay 'modification-hooks '(flycheck-overlay--clear-overlay-on-modification))))

(defun flycheck-overlay--clear-overlay-on-modification (overlay &rest _)
  "Clear OVERLAY when the buffer is modified."
  (delete-overlay overlay))

(defun flycheck-overlay--create-overlay-string (col-pos virtual-line indicator marked-string existing-bg)
  "Create the overlay string.
Based on COL-POS, INDICATOR, MARKED-STRING, and EXISTING-BG."
  (flycheck-overlay--mark-all-symbols
   :input (if flycheck-overlay-show-at-eol
              (concat indicator marked-string)
            (concat (make-string col-pos ?\s) virtual-line indicator marked-string))
   :regex flycheck-overlay-regex-mark-parens
   :property `(:inherit flycheck-overlay-marker :background ,existing-bg)))

(defun replace-curly-quotes (text)
  "Replace curly quotes with straight quotes in TEXT."
  (replace-regexp-in-string "[“”]" "\""
    (replace-regexp-in-string "[‘’]" "'" text)))

(cl-defun flycheck-overlay--mark-all-symbols (&key input regex property)
  "Highlight all symbols matching REGEX in INPUT with specified PROPERTY."
  (save-match-data
    (setq input (replace-curly-quotes input))  ; Replace curly quotes with straight quotes
    (let ((pos 0))
      (while (string-match regex input pos)
        (let* ((start (match-beginning 1))
               (end (match-end 1))
               (existing-face (text-properties-at start input))
               (new-face (append existing-face (list 'face property))))
          (add-text-properties start end new-face input)
          (setq pos end))))
    input))

(defun flycheck-overlay-errors-at (pos)
  "Return the Flycheck errors at POS."
  (delq nil (mapcar (lambda (ov)
                      (overlay-get ov 'flycheck-overlay))
                    (overlays-at pos))))

(defun flycheck-display-error-at-point ()
  "Display the Flycheck error at point, if any."
  (let ((errors (flycheck-overlay-errors-at (point))))
    (when errors
      (let ((messages (delq nil (mapcar (lambda (err)
                                          (when (flycheck-error-p err)
                                            (flycheck-error-format-message-and-id err)))
                                        errors))))
        (when messages
          (flycheck-display-error-messages messages))))))


(defun flycheck-overlay--remove-checker-name (msg)
  "Remove checker name prefix from (as MSG).
If it appears at the start.
Ignores colons that appear within quotes or parentheses."
  (when flycheck-overlay-hide-checker-name
    (let ((case-fold-search nil))
      ;; Match start of string, followed by any characters except quotes/parens,
      ;; followed by a colon, capturing everything after
      (if (string-match flycheck-overlay-checker-regex (replace-curly-quotes msg))
          (setq msg (string-trim (match-string 1 msg))))))
  msg)

(defun flycheck-overlay--display-errors (&optional errors)
  "Display ERRORS using overlays."
  (condition-case display-err
      (let ((errs (flycheck-overlay--sort-errors (or errors flycheck-current-errors))))
        (when (listp errs)
          (flycheck-overlay--clear-overlays)  ; Clear existing overlays
          (dolist (err (delq nil errs))  ; Filter out nil values
            (when (flycheck-error-p err)  ; Ensure err is a valid flycheck-error
              (condition-case err-handler
                  (let* ((level (flycheck-error-level err))
                         (msg (flycheck-overlay--remove-checker-name (flycheck-error-message err)))
                         (region (flycheck-overlay--get-error-region err)))
                    (when flycheck-overlay-debug
                      (message "Debug: level=%S msg=%S region=%S" level msg region))
                    (when (and region (car region) (cdr region) msg)
                      (let ((overlay (flycheck-overlay--create-overlay region level msg)))
                        (when overlay
                          (push overlay flycheck-overlay--overlays)))))
                (error
                 (message "Debug: Error handling individual error: %S" err-handler)))))))
    (error
     (message "Debug: Top-level display error: %S" display-err))))

(defun flycheck-overlay--clear-overlays ()
  "Remove all flycheck overlays from the current buffer."
  (dolist (ov flycheck-overlay--overlays)
    (when (overlayp ov)
      (delete-overlay ov)))
  (setq flycheck-overlay--overlays nil)
  (remove-overlays (point-min) (point-max) 'flycheck-overlay t)
  (flycheck-delete-all-overlays))

;;;###autoload
(define-minor-mode flycheck-overlay-mode
  "Minor mode for displaying Flycheck errors using overlays."
  :lighter " fo"
  :group 'flycheck-overlay
  (if flycheck-overlay-mode
      (flycheck-overlay--enable)
    (flycheck-overlay--disable)))

(defun flycheck-overlay--enable ()
  "Enable Flycheck overlay mode."
  (add-hook 'flycheck-after-syntax-check-hook
            #'flycheck-overlay--maybe-display-errors-debounced nil t)
  (add-hook 'after-change-functions
            #'flycheck-overlay--handle-buffer-changes nil t)
  (when flycheck-current-errors
    (flycheck-overlay--maybe-display-errors-debounced)))

(defun flycheck-overlay--disable ()
  "Disable Flycheck overlay mode."
  (when flycheck-overlay--debounce-timer
    (cancel-timer flycheck-overlay--debounce-timer)
    (setq flycheck-overlay--debounce-timer nil))
  (remove-hook 'flycheck-after-syntax-check-hook
               #'flycheck-overlay--maybe-display-errors-debounced t)
  (remove-hook 'after-change-functions
               #'flycheck-overlay--handle-buffer-changes t)
  (setq flycheck-overlay--debounce-timer nil)
  (save-restriction
    (widen)
    (flycheck-overlay--clear-overlays)))

(defun flycheck-overlay--maybe-display-errors ()
  "Display errors except on current line."
  (unless (buffer-modified-p)
    (let ((current-line (line-number-at-pos)))
      (flycheck-overlay--display-errors)
      (when flycheck-overlay-hide-when-cursor-is-on-same-line
      (dolist (ov flycheck-overlay--overlays)
        (when (and (overlayp ov)
                   (= (line-number-at-pos (overlay-start ov)) current-line))
          (delete-overlay ov)))))))

(defun flycheck-overlay--maybe-display-errors-debounced ()
  "Debounced version of `flycheck-overlay--maybe-display-errors`."
  (condition-case err
      (progn
        (when flycheck-overlay--debounce-timer
          (cancel-timer flycheck-overlay--debounce-timer))
        (setq flycheck-overlay--debounce-timer
              (run-with-idle-timer flycheck-overlay-debounce-interval nil
                                   #'flycheck-overlay--maybe-display-errors)))
    (error
     (message "Error in debounced display: %S" err)
     (setq flycheck-overlay--debounce-timer nil))))

(defun flycheck-overlay--handle-buffer-changes (&rest _)
  "Handle buffer modifications by clearing overlays on the current line while editing."
  (condition-case err
      (when (buffer-modified-p)
        (let ((current-line (line-number-at-pos)))
          (dolist (ov flycheck-overlay--overlays)
            (when (and (overlayp ov)
                       (let ((ov-line (line-number-at-pos (overlay-start ov))))
                         (= ov-line current-line)))
              (delete-overlay ov)))
          (setq flycheck-overlay--overlays
                (cl-remove-if-not #'overlay-buffer flycheck-overlay--overlays))))
    (error
     (message "Error in flycheck-overlay--handle-buffer-changes: %S" err))))

(defun flycheck-overlay--color-to-rgb (color)
  "Convert COLOR (hex or name) to RGB components."
  (let ((rgb (color-values color)))
    (if rgb
        (mapcar (lambda (x) (/ x 256)) rgb)
      (error "Invalid color: %s" color))))

(defun flycheck-overlay--rgb-to-hex (r g b)
  "Convert R G B components to hex color string."
  (format "#%02x%02x%02x" r g b))

(defun flycheck-overlay--darken-color (color percent)
  "Darken COLOR by PERCENT."
  (let* ((rgb (flycheck-overlay--color-to-rgb color))
         (darkened (mapcar (lambda (component)
                            (min 255
                                 (floor (* component (- 100 percent) 0.01))))
                          rgb)))
    (apply 'flycheck-overlay--rgb-to-hex darkened)))

(provide 'flycheck-overlay)
;;; flycheck-overlay.el ends here
