# flycheck-overlay

A modern, aesthetic overlay display for Flycheck errors in Emacs. Flycheck by displaying errors, warnings, and information messages using customizable overlays.

## Features

- 🎨 Beautiful, customizable overlays for error display
- 🚦 Different styles for errors, warnings, and info messages
- 🔄 Real-time overlay updates while editing
- 💡 Smart positioning and formatting of error messages
- 🎯 Efficient overlay management
- 📝 Markdown-style syntax highlighting in messages

## Screenshots

[Add screenshots here]

## Installation

### Using MELPA (recommended)

```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(package-install 'flycheck-overlay)
```

### Using use-package

```elisp
(use-package flycheck-overlay
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-overlay-mode))
```

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

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Requirements

- Emacs 25.1 or higher
- Flycheck 0.23 or higher


## Acknowledgments

- Thanks to the Flycheck team for the excellent error checking framework
- Inspired by various overlay-based error display implementations

## Author

Mikael Konradsson <mikael.konradsson@outlook.com>
```
