# flycheck-overlay

❤️ [Please sponsor me if you like this package](https://github.com/sponsors/konrad1977)

A modern, aesthetic overlay display for Flycheck errors in Emacs. Flycheck by displaying errors, warnings, and information messages using customizable overlays.

<p align="center">
  <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/overlay_1.png" alt="Screenshot of overlay-usage for Emacs."/>
</p>

## Features

- 🎨 Beautiful, customizable overlays for error display
- 🚦 Different styles for errors, warnings, and info messages
- 🔄 Real-time overlay updates while editing
- 💡 Smart positioning and formatting of error messages
- 🎯 Efficient overlay management
- 📝 Markdown-style syntax highlighting in messages

## Screenshots

<p align="center">
  <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/overlay_2.png" alt="Screenshot of overlay-usage for Emacs."/>
  <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/overlay_3.png" alt="Screenshot of overlay-usage for Emacs."/>
</p>

## Installation

### Manual Installation

1. Download `flycheck-overlay.el`
2. Add to your load path:
```elisp
(add-to-list 'load-path "/path/to/flycheck-overlay")
(require 'flycheck-overlay)
```

## Configuration

### Basic Setup

```elisp
;; Enable flycheck-overlay-mode globally
(add-hook 'flycheck-mode-hook #'flycheck-overlay-mode)
```

### Customizing Faces

You can customize the appearance of overlays by modifying these faces:

```elisp
(custom-set-faces
 '(flycheck-overlay-error
   ((t :background "#453246"
       :foreground "#ea8faa"
       :height 0.9
       :weight normal)))
 
 '(flycheck-overlay-warning
   ((t :background "#331100"
       :foreground "#DCA561"
       :height 0.9
       :weight normal)))
 
 '(flycheck-overlay-info
   ((t :background "#374243"
       :foreground "#a8e3a9"
       :height 0.9
       :weight normal))))
```

## Usage

Once enabled, `flycheck-overlay` will automatically display error messages as overlays below the corresponding line. The overlays will:

- Show errors in red with appropriate background
- Display warnings in yellow/orange
- Show information messages in green
- Highlight code snippets and symbols in messages
- Update in real-time as you type
- Clear automatically when errors are fixed


## Flycheck Overlay Icons

You can customize the icons used for different types of Flycheck messages in the overlay display. These settings allow you to define custom icons for information, warning, and error messages.

### Icon settings

```elisp
(defcustom flycheck-overlay-info-icon " "
  "Icon used for information.")

(defcustom flycheck-overlay-warning-icon " "
  "Icon used for warnings.")

(defcustom flycheck-overlay-error-icon " "
  "Icon used for errors.")
  
(defcustom flycheck-overlay-show-at-eol nil
  "Show error messages at the end of the line."
  :type 'boolean
  :group 'flycheck-overlay)

(setq flycheck-overlay-info-icon "🛈")
(setq flycheck-overlay-warning-icon "⚠")
(setq flycheck-overlay-error-icon "✘")
```

### Visibility settings

```elisp
(defcustom flycheck-overlay-hide-when-cursor-is-on-same-line nil
  "Hide error messages when the cursor is on the same line."
  :type 'boolean
  :group 'flycheck-overlay)
  
;; Custom icons using different Unicode characters
(setq flycheck-overlay-show-at-eol t) ;; show at end of the line instead.
(setq flycheck-overlay-hide-when-cusor-is-on-same-line t) ;;; Hide overlay when cursor is at same line good for shot-at-eol.
```

### Show or hide checker name

<p align="center">
  <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/checker_name.png" alt="Screenshot of overlay-usage for Emacs."/>
  <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/no_checker_name.png" alt="Screenshot of overlay-usage for Emacs."/>
</p>

### Show under or after the faulty code

<p align="center">
  <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/eol.png" alt="Screenshot of overlay-usage for Emacs."/>
  <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/under.png" alt="Screenshot of overlay-usage for Emacs."/>
</p>

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request


## Acknowledgments

- Thanks to the Flycheck team for the excellent error checking framework
- Inspired by various overlay-based error display implementations

## Author

Mikael Konradsson <mikael.konradsson@outlook.com>
  
