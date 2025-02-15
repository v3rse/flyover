;;; flycheck-overlay.el --- Display Flycheck errors with overlays -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Mikael Konradsson <mikael.konradsson@outlook.com>
;; Version: 0.5.0
;; Package-Requires: ((emacs "27.1") (flycheck "0.23"))
;; Keywords: convenience, tools
;; URL: https://github.com/yourusername/flycheck-overlay

;;; Commentary:
;; This package provides a way to display Flycheck errors using overlays.
;; It offers customization options for icons, colors, and display behavior.

;;; Code:

(define-advice flycheck-overlays-in (:override (beg end) disable-sorting)
  "Get flycheck overlays between BEG and END without sorting."
  (seq-filter (lambda (ov) (overlay-get ov 'flycheck-overlay))
              (overlays-in beg end)))

(require 'flycheck)
(require 'flymake)
(require 'cl-lib)

(defgroup flycheck-overlay nil
  "Display Flycheck/Flymake errors using overlays."
  :prefix "flycheck-overlay-"
  :group 'tools)

(defcustom flycheck-overlay-checkers '(flycheck flymake)
  "Checkers to use for displaying errors.
Supported values are `flycheck` and `flymake`."
  :type '(set (const :tag "Flycheck" flycheck)
              (const :tag "Flymake" flymake))
  :group 'flycheck-overlay)

(defvar-local flycheck-overlay--overlays nil
  "List of overlays used in the current buffer.")

(defvar flycheck-overlay--debounce-timer nil
  "Timer used for debouncing error checks.")

(defvar flycheck-overlay-regex-mark-quotes  "\\('[^']+'\\|\"[^\"]+\"\\|\{[^\}]+\}\\)"
  "Regex to match quoted strings or everything after a colon.")

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
  "Show a virtual line to highlight the error a bit more."
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

(defun flycheck-overlay--get-flymake-diagnostics ()
  "Get all current Flymake diagnostics for this buffer."
  (when (and (memq 'flymake flycheck-overlay-checkers)
             (bound-and-true-p flymake-mode))
    (flymake-diagnostics)))

(defun flycheck-overlay--convert-flymake-diagnostic (diag)
  "Convert a Flymake DIAG to Flycheck error format."
  (let* ((beg (flymake-diagnostic-beg diag))
         (type (flymake-diagnostic-type diag))
         (text (flymake-diagnostic-text diag))
         (level (pcase type
                 ('flymake-error 'error)
                 ('flymake-warning 'warning)
                 ('flymake-note 'info)
                 (_ 'warning))))
    (flycheck-error-new-at
     (line-number-at-pos beg)
     (save-excursion
       (goto-char beg)
       (current-column))
     level
     text)))

(defun flycheck-overlay--get-all-errors ()
  "Get all errors from enabled checkers."
  (let (all-errors)
    (when (memq 'flycheck flycheck-overlay-checkers)
      (setq all-errors (append all-errors flycheck-current-errors)))
    (when (memq 'flymake flycheck-overlay-checkers)
      (setq all-errors
            (append all-errors
                    (mapcar #'flycheck-overlay--convert-flymake-diagnostic
                            (flycheck-overlay--get-flymake-diagnostics)))))
    all-errors))

(defun flycheck-overlay--sort-errors (errors)
  "Safely sort ERRORS by their buffer positions.
This function filters out invalid errors and sorts the remaining ones."
  (condition-case sort-err
      (when (and errors (listp errors))
        (let ((valid-errors
               (seq-filter
                (lambda (err)
                  (and err
                       (not (eq err t))
                       (flycheck-error-p err)
                       (let ((line (flycheck-error-line err))
                             (column (flycheck-error-column err)))
                         (and (numberp line) (>= line 0)
                              (or (not column)
                                  (and (numberp column) (>= column 0)))))))
                errors)))
          (when flycheck-overlay-debug
            (message "Debug: Valid errors: %S" valid-errors))
          (sort valid-errors
                (lambda (a b)
                  (let ((line-a (flycheck-error-line a))
                        (line-b (flycheck-error-line b)))
                    (or (< line-a line-b)
                        (and (= line-a line-b)
                             (let ((col-a (or (flycheck-error-column a) 0))
                                   (col-b (or (flycheck-error-column b) 0)))
                               (< col-a col-b)))))))))
    (error
     (when flycheck-overlay-debug
       (message "Debug: Error sorting errors: %S for input: %S" sort-err errors))
     nil)))

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
  "Get the start and end position for ERR."
  (condition-case region-err
      (progn
        (let* ((line (flycheck-error-line err))
               (column (or (flycheck-error-column err) 0)) ; Sätt till 0 om nil
               (start-pos (progn
                           (save-excursion
                             (goto-char (point-min))
                             (forward-line (1- line))
                             (when (> column 0)
                               (move-to-column column))
                             (point))))
               (end-pos (progn
                         (save-excursion
                           (goto-char start-pos)
                           (line-end-position)))))
          (when (and (numberp start-pos) (numberp end-pos))
            (cons start-pos end-pos))))
    (error
     (message "Error in get-error-region: %S" region-err)
     nil)))

(defun flycheck-overlay--create-overlay (region level msg &optional error)
  "Create an overlay at REGION with LEVEL and message MSG.
REGION should be a cons cell (BEG . END) of buffer positions.
LEVEL is the error level (error, warning, or info).
ERROR is the optional original flycheck error object."
  (let ((overlay nil))
    (condition-case ov-err
        (let* ((beg (car region))
               (end (cdr region)))
          (save-excursion
            (goto-char (min end (point-max)))
            (let* ((next-line-beg (if flycheck-overlay-show-at-eol
                                    end
                                    (line-beginning-position 2)))
                   (face (flycheck-overlay--get-face level)))
              (when (and (numberp beg) 
                        (numberp end)
                        (numberp next-line-beg)
                        (> beg 0)
                        (> end 0)
                        (> next-line-beg 0)
                        (<= beg (point-max))
                        (<= end (point-max))
                        (<= next-line-beg (point-max)))
                (setq overlay (make-overlay beg next-line-beg))
                (when (overlayp overlay)
                  (flycheck-overlay--configure-overlay overlay face msg beg error))))))
      (error
       (when flycheck-overlay-debug
         (message "Error creating overlay: %S for region: %S level: %S msg: %S"
                  ov-err region level msg))))
    overlay))

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

(defun flycheck-overlay--create-basic-overlay-string (msg face)
  "Create basic overlay string with MSG and FACE."
  (propertize (concat " " msg " ")
              'face face
              'rear-nonsticky t))

(defun flycheck-overlay--get-virtual-line (face)
  "Get virtual line with proper face properties."
  (when flycheck-overlay-show-virtual-line
    (let ((fg-color (face-foreground face nil t)))
      (propertize (flycheck-overlay-get-arrow)
                  'face `(:foreground ,fg-color)
                  'rear-nonsticky t))))

(defun flycheck-overlay--configure-overlay (overlay face msg beg error)
  "Configure OVERLAY with FACE, MSG, and BEG and ERROR."
  (condition-case err
      (when (overlayp overlay)
        ;; Sätt grundläggande overlay properties
        (overlay-put overlay 'flycheck-overlay t)
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'priority 1000)
        (overlay-put overlay 'modification-hooks 
                     '(flycheck-overlay--clear-overlay-on-modification))
        
        (let* ((col-pos (progn
                         (save-excursion
                           (goto-char (or beg (point-min)))
                           (current-column))))
               (existing-bg (face-background face nil t))
               (indicator (flycheck-overlay--get-indicator face))
               (display-msg (concat " " msg " "))
               (virtual-line (when flycheck-overlay-show-virtual-line
                             (propertize (flycheck-overlay-get-arrow)
                                       'face `(:foreground ,(face-foreground face nil t)))))
               (display-string (propertize display-msg
                                         'face face
                                         'cursor-sensor-functions nil
                                         'cursor-intangible t
                                         'rear-nonsticky t))

               (marked-string (flycheck-overlay--mark-all-symbols
                             :input display-string
                             :regex flycheck-overlay-regex-mark-quotes
                             :property `(:inherit flycheck-overlay-marker 
                                                  :background ,existing-bg)))

               (overlay-string (progn
                               (flycheck-overlay--create-overlay-string 
                                col-pos virtual-line indicator marked-string existing-bg)))
               (final-string (if (and flycheck-overlay-show-at-eol
                                    (< (+ col-pos (length display-msg)) (window-width)))
                               overlay-string
                             (concat overlay-string "\n"))))
          
          (overlay-put overlay 'after-string 
                       (propertize final-string
                                  'rear-nonsticky t
                                  'cursor-sensor-functions nil
                                  'cursor-intangible t))
          
          (when (flycheck-error-p error)
            (overlay-put overlay 'flycheck-error error))))
    (error
     (message "Error in configure-overlay: %S for beg=%S col-pos=%S" 
              err beg (when (boundp 'col-pos) col-pos)))))

(defun flycheck-overlay--clear-overlay-on-modification (overlay &rest _)
  "Clear OVERLAY when the buffer is modified."
  (delete-overlay overlay))

(defun flycheck-overlay--create-overlay-string (col-pos virtual-line indicator marked-string existing-bg)
  "Create the overlay string.
COL-POS is the column position.
VIRTUAL-LINE is the line indicator.
INDICATOR is the error/warning icon.
MARKED-STRING is the message with marked symbols.
EXISTING-BG is the background color."
  (when flycheck-overlay-debug
    (message "Debug overlay-string: starting with col-pos=%S" col-pos))
  (let ((result-string
         (if flycheck-overlay-show-at-eol
             (concat indicator marked-string)
           (concat (make-string (or col-pos 0) ?\s) 
                  virtual-line 
                  indicator 
                  marked-string))))

    (when flycheck-overlay-debug
      (message "Debug overlay-string: created string successfully"))
    (flycheck-overlay--mark-all-symbols
     :input result-string
     :regex flycheck-overlay-regex-mark-parens
     :property `(:inherit flycheck-overlay-marker :background ,existing-bg))))

(defun flycheck-overlay-replace-curly-quotes (text)
  "Replace curly quotes with straight quotes in TEXT."
  (replace-regexp-in-string "[“”]" "\""
    (replace-regexp-in-string "[‘’]" "'" text)))

(cl-defun flycheck-overlay--mark-all-symbols (&key input regex property)
  "Highlight all symbols matching REGEX in INPUT with specified PROPERTY."
  (save-match-data
    (setq input (flycheck-overlay-replace-curly-quotes input))  ; Replace curly quotes with straight quotes
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
                      (when-let* ((err (overlay-get ov 'flycheck-error)))
                        (when (flycheck-error-p err)
                          err)))
                    (overlays-at pos))))

(defun flycheck-overlay--remove-checker-name (msg)
  "Remove checker name prefix from (as MSG).
If it appears at the start.
Ignores colons that appear within quotes or parentheses."
  (when flycheck-overlay-hide-checker-name
    (let ((case-fold-search nil))
      ;; Match start of string, followed by any characters except quotes/parens,
      ;; followed by a colon, capturing everything after
      (if (string-match flycheck-overlay-checker-regex (flycheck-overlay-replace-curly-quotes msg))
          (setq msg (string-trim (match-string 1 msg))))))
  msg)

(defun flycheck-overlay--display-errors (&optional errors)
  "Display ERRORS using overlays."
  (condition-case display-err
      (let ((errs (flycheck-overlay--sort-errors (or errors (flycheck-overlay--get-all-errors)))))
        (when errs
          (flycheck-overlay--clear-overlays)
          (dolist (err errs)
            (when (flycheck-error-p err)
              (let* ((level (flycheck-error-level err))
                     (msg (flycheck-error-message err))
                     (line (flycheck-error-line err))
                     (col (flycheck-error-column err)))
                (when-let* ((msg (and msg (flycheck-overlay--remove-checker-name msg)))
                           (region (flycheck-overlay--get-error-region err)))
                  (when-let* ((overlay (flycheck-overlay--create-overlay 
                                     region level msg err)))
                    (push overlay flycheck-overlay--overlays))))))))
    (error
     (when flycheck-overlay-debug
       (message "Debug: Display error: %S" display-err)))))

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
  "Enable Flycheck/Flymake overlay mode."
  (when (memq 'flycheck flycheck-overlay-checkers)
    (add-hook 'flycheck-after-syntax-check-hook
              #'flycheck-overlay--maybe-display-errors-debounced nil t))
  (when (memq 'flymake flycheck-overlay-checkers)
    (add-hook 'flymake-after-diagnostics-hook  ; Changed from flymake-diagnostic-functions
              #'flycheck-overlay--maybe-display-errors-debounced nil t))
  (add-hook 'after-change-functions
            #'flycheck-overlay--handle-buffer-changes nil t)
  ;; Force initial display of existing errors
  (flycheck-overlay--maybe-display-errors))

(defun flycheck-overlay--disable ()
  "Disable Flycheck/Flymake overlay mode."
  (when flycheck-overlay--debounce-timer
    (cancel-timer flycheck-overlay--debounce-timer)
    (setq flycheck-overlay--debounce-timer nil))
  (when (memq 'flycheck flycheck-overlay-checkers)
    (remove-hook 'flycheck-after-syntax-check-hook
                 #'flycheck-overlay--maybe-display-errors-debounced t))
  (when (memq 'flymake flycheck-overlay-checkers)
    (remove-hook 'flymake-diagnostic-functions
                 #'flycheck-overlay--maybe-display-errors-debounced t))
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
            (when (and ov 
                      (overlayp ov)
                      (overlay-buffer ov)  ; Check if overlay is still valid
                      (let ((ov-line (line-number-at-pos (overlay-start ov))))
                        (= ov-line current-line)))
              (delete-overlay ov)))
          (setq flycheck-overlay--overlays
                (cl-remove-if-not (lambda (ov)
                                   (and (overlayp ov)
                                        (overlay-buffer ov)))
                                 flycheck-overlay--overlays))))
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
