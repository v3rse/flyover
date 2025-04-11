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

### Theme Integration

```elisp
;; Use theme colors for error/warning/info faces
(setq flycheck-overlay-use-theme-colors t)

;; Adjust background lightness (lower values = darker)
(setq flycheck-overlay-background-lightness 45)

;; Make icon background darker than foreground
(setq flycheck-overlay-percent-darker 40)

(setq flycheck-overlay-text-tint 'lighter) ;; or 'darker or nil

;; "Percentage to lighten or darken the text when tinting is enabled."
(setq flycheck-overlay-text-tint-percent 50)
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

### Checker Configuration

```elisp
;; Choose which checkers to use (flycheck, flymake, or both)
(setq flycheck-overlay-checkers '(flycheck flymake))

;; Enable debug messages
(setq flycheck-overlay-debug nil)
```

### Optimization settings

```elisp
;; Time in seconds to wait before checking and displaying errors after a change
(setq flycheck-overlay-debounce-interval 0.2) 
```

## Flycheck Overlay Icons

You can customize the icons used for different types of Flycheck messages in the overlay display. These settings allow you to define custom icons for information, warning, and error messages.

### Icon settings

```elisp
;;; Icons
(setq flycheck-overlay-info-icon "🛈")
(setq flycheck-overlay-warning-icon "⚠")
(setq flycheck-overlay-error-icon "✘")

;;; Icon padding

;;; You might want to adjust this setting if you icons are not centererd or if you more or less space.fs
(setq flycheck-overlay-icon-left-padding 0.9)
(setq flycheck-overlay-icon-right-padding 0.9)
```

### Customizing Error Indicators

You can customize the appearance of the error indicators using various line and arrow styles through the `flycheck-overlay-virtual-line-type` variable:

```elisp
(setq flycheck-overlay-virtual-line-type 'curved-dotted-arrow)

;;; Overide virtual-line-type with your own
(setq flycheck-overlay-virtual-line-icon "╰──") ;;; default its nil
```

| Style Name | Display | Description |
|------------|----------|-------------|
| `nil` | | No indicator |
| `line-no-arrow` | └── | Straight line |
| `curved-line-no-arrow` | ╰── | Curved line |
| `double-line-no-arrow` | ╚══ | Double line |
| `bol` | ┗━━ | Bold line |
| `dotted-line-no-arrow` | └┈┈ | Dotted line |
| `straight-arrow` | └──► | Straight line with arrow |
| `double-line-arrow` | ╚══► | Double line with arrow |
| `bold-arrow` | ┗━━► | Bold line with arrow |
| `dotted-arrow` | └┈┈► | Dotted line with arrow |
| `curved-arrow` | ╰──► | Curved line with arrow (default) |
| `curved-bold-arrow` | ╰━━► | Curved bold line with arrow |
| `curved-double-arrow` | ╰══► | Curved double line with arrow |
| `curved-dotted-arrow` | ╰┈┈► | Curved dotted line with arrow |


### Visibility settings

```elisp
;;; Hide checker name for a cleaner UI
(setq flycheck-overlay-hide-checker-name t) 

;;; show at end of the line instead.
(setq flycheck-overlay-show-at-eol t) 

;;; Hide overlay when cursor is at same line good for shot-at-eol.
(setq flycheck-overlay-hide-when-cusor-is-on-same-line t) 

;;; Show an arrow (or icon of your choice) before the error to highlight the error a bit more.
(setq flycheck-overlay-show-virtual-line t)

;;; Hide the flychek-overlay when cursor it at the same line
(setq flycheck-overlay-hide-when-cursor-is-on-same-line t)  ;; default t

```

<p align="center">
  <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/flycheck_hide_overlay_cursor.gif" alt="Gif of showing hide cursor is on same line"/>
</p>

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
  
